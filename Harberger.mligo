(* FA2 non-fungible with a weekly Harberger tax *)

(* FA2 interface *)

type token_id = nat
 
type transfer_destination =
[@layout:comb]
{
  to_: address;
  token_id: token_id;
  amount: nat;
}

type transfer =
[@layout:comb]
{
  from_: address;
  txs: transfer_destination list;
}

type balance_of_request =
[@layout:comb]
{
  owner: address;
  token_id: token_id;
}

type balance_of_response =
[@layout:comb]
{
  request: balance_of_request;
  balance: nat;
}

type balance_of_param =
[@layout:comb]
{
  requests: balance_of_request list;
  callback: (balance_of_response list) contract;
}

type operator_param =
[@layout:comb]
{
  owner: address;
  operator: address;
  token_id: token_id;
}

type update_operator =
[@layout:comb]
| Add_operator of operator_param
| Remove_operator of operator_param

(* Contract interface *)

type mint =
[@layout:comb]
{
  to_: address;
  token_id: token_id;
  name: string;
  tax: nat;
  tax_interval: nat;
  tax_recipient: address;
  extras: (string, string) map;
  price: tez;
}

type forced_sale_origin =
[@layout:comb]
{
  from_: address;
  token_id: token_id;
}

type forced_sale =
[@layout:comb]
{
  to_: address;
  txs: forced_sale_origin list;
}

type tax_param =
[@layout:comb]
{
  owner: address;
  token_id: token_id;
  amount: tez;
}

type tax_claim_origin =
[@layout:comb]
{
  from_: address;
  token_id: token_id;
  amount: tez;
}

type tax_claim =
[@layout:comb]
{
  to_: address;
  txs: tax_claim_origin list;
}

type price_set =
[@layout:comb]
{
  token_id: token_id;
  price: tez;
}

type price_update =
[@layout:comb]
{
  owner: address;
  txs: price_set list;
}

(* Storage *)

type token_metadata = {
  name: string;
  tax: nat;
  tax_interval: nat;
  tax_recipient: address;
  extras: (string, string) map;
}

type price = {
  current: tez;
  minimum: tez;
}

type tax_record = {
  value: tez;
  start_date: timestamp;
  end_date: timestamp option;
}

type ledger = (address, token_id set) big_map

type operator_storage = ((address * address * token_id), unit) big_map

type token_metadata_storage = (token_id, token_metadata) big_map

type token_price_storage = ((address * token_id), price) big_map

type tax_deposit_storage = (address, tez) big_map

type tax_record_storage = ((address * token_id), tax_record list) big_map

type tax_claim_storage = ((address * token_id), tez) big_map

type storage = {
  ledger: ledger;
  operators: operator_storage;
  token_metadata: token_metadata_storage;
  token_prices: token_price_storage;
  tax_deposits: tax_deposit_storage;
  tax_records: tax_record_storage;
  tax_claims: tax_claim_storage;
}

(* Helper functions *)

[@inline]
let find_tokens (owner, ledger: address * ledger): token_id set =
  match Big_map.find_opt owner ledger with
  | None -> Set.empty
  | Some tokens -> tokens

[@inline]
let find_metadata (token_id, token_metadata: token_id * token_metadata_storage): token_metadata =
  match Big_map.find_opt token_id token_metadata with
  | None -> (failwith "token undefined" : token_metadata)
  | Some metadata -> metadata

[@inline]
let find_price (key, token_prices: (address * token_id) * token_price_storage): price =
  match Big_map.find_opt key token_prices with
  | None -> {
      current = 0tez;
      minimum = 0tez;
    }
  | Some price -> price

[@inline]
let find_tax_deposit (owner, tax_deposits: address * tax_deposit_storage): tez =
  match Big_map.find_opt owner tax_deposits with
  | None -> 0tez
  | Some deposit -> deposit

[@inline]
let find_records (key, tax_records: (address * token_id) * tax_record_storage): tax_record list =
  match Big_map.find_opt key tax_records with
  | None -> []
  | Some record_list -> record_list

[@inline]
let find_claimed (key, tax_claims: (address * token_id) * tax_claim_storage): tez =
  match Big_map.find_opt key tax_claims with
  | None -> 0tez
  | Some claimed -> claimed

let is_operator (owner, token_id, operators: address * token_id * operator_storage): bool =
  if Tezos.sender = owner
  then true
  else if Big_map.mem (owner, Tezos.sender, token_id) operators
  then true
  else (failwith "FA2_NOT_OPERATOR" : bool)

let add_token (owner, token_id, ledger: address * token_id * ledger): ledger =
  let tokens = find_tokens (owner, ledger) in
  let next_tokens = Set.add token_id tokens in
  Big_map.update owner (Some next_tokens) ledger

let remove_token (owner, token_id, ledger: address * token_id * ledger): ledger =
  let tokens = find_tokens (owner, ledger) in
  if Set.mem token_id tokens then
    let next_tokens = Set.remove token_id tokens in
    if Set.size next_tokens = 0n
    then Big_map.remove owner ledger
    else Big_map.update owner (Some next_tokens) ledger
  else
    (failwith "FA2_INSUFFICIENT_BALANCE" : ledger)

let start_tax (owner, token_id, token_prices, tax_records
    : address * token_id * token_price_storage * tax_record_storage): tax_record_storage =
  let key = owner, token_id in
  let record_list = find_records (key, tax_records) in
  let price = find_price (key, token_prices) in
  let record = {
    value = price.current;
    start_date = Tezos.now;
    end_date = (None : timestamp option);
  } in
  let next_record_list = record :: record_list in
  Big_map.update key (Some next_record_list) tax_records

let end_tax (owner, token_id, token_prices, tax_records
    : address * token_id * token_price_storage * tax_record_storage): tax_record_storage =
  let key = owner, token_id in
  let record_list = find_records (key, tax_records) in
  let price = find_price (key, token_prices) in
  let next_record_list =
    List.fold
      (fun (r, record: tax_record list * tax_record) ->
        if record.value = price.current && record.end_date = (None : timestamp option) then
          let end_record = {record with end_date = Some Tezos.now} in
          end_record :: r
        else
          record :: r
      ) record_list ([] : tax_record list)
  in
  Big_map.update key (Some next_record_list) tax_records

(* Calculate how much weekly tax *)
let calculate_spent_tax (owner, token_id, store: address * token_id * storage): tez =
  let key = owner, token_id in
  let record_list = find_records (key, store.tax_records) in
  let metadata = find_metadata (token_id, store.token_metadata) in
  let spent =
    List.fold
      (fun (spent, record: tez * tax_record) ->
        let end_date =
          match record.end_date with
          | None -> Tezos.now
          | Some date -> date
        in
        let time_passed = abs (end_date - record.start_date) in
        let tax_amount = record.value * metadata.tax / 1000n in
        let added = tax_amount * time_passed / metadata.tax_interval in
        spent + added
      ) record_list 0tez
  in
  let claimed = find_claimed (key, store.tax_claims) in
  spent - claimed

(* Entry points *)

type entry_points =
| Transfer of transfer list
| Balance_of of balance_of_param
| Update_operators of update_operator list
| Mint_token of mint list
| Force_sale of forced_sale list
| Deposit_tax of tax_param
| Withdraw_tax of tax_param
| Claim_tax of tax_claim list
| Update_price of price_update list

type return = operation list * storage

(* Operator transfers tokens in batch to recipients *)
let transfer (transfers, store: transfer list * storage): return =
  let process_transfer (s, tx: storage * transfer) =
    List.fold
      (fun (s, dest: storage * transfer_destination) ->
        if dest.amount > 1n then
          (failwith "total supply of non-fungible cannot exceed one" : storage)
        else if dest.amount = 1n then
          let _ok = is_operator (tx.from_, dest.token_id, s.operators) in
          let l = remove_token (tx.from_, dest.token_id, s.ledger) in
          let next_ledger = add_token (dest.to_, dest.token_id, l) in
          let r = end_tax (tx.from_, dest.token_id, s.token_prices, s.tax_records) in
          let price = find_price ((tx.from_, dest.token_id), s.token_prices) in
          let next_token_prices = Big_map.update (dest.to_, dest.token_id) (Some price) s.token_prices in
          let next_tax_records = start_tax (dest.to_, dest.token_id, next_token_prices, r) in
          { s with
            ledger = next_ledger;
            token_prices = next_token_prices;
            tax_records = next_tax_records }
        else
          s
      ) tx.txs s
  in
  let next_store = List.fold process_transfer transfers store in
  ([]: operation list), next_store

(* Send balances to callback in batches *)
let balances (param, store: balance_of_param * storage): return =
  let to_balance (r: balance_of_request) =
    if not Big_map.mem r.token_id store.token_metadata then
      (failwith "FA2_TOKEN_UNDEFINED" : balance_of_response)
    else
      let tokens = find_tokens (r.owner, store.ledger) in
      let bal =
        if Set.mem r.token_id tokens
        then 1n
        else 0n
      in
      {request = r; balance = bal}
  in
  let responses = List.map to_balance param.requests in
  let op = Tezos.transaction responses 0mutez param.callback in
  [op], store

(* Token owners change operators in batches *)
let update_operators (updates, store: update_operator list * storage): return =
  let process_update (ops, update: operator_storage * update_operator) =
    let validate_owner (op: operator_param): bool =
      if op.owner = Tezos.sender then
        true
      else
        (failwith "FA2_NOT_OWNER")
    in
    match update with
    | Add_operator op ->
      let _ok = validate_owner op in
      let key = (op.owner, op.operator, op.token_id) in
      Big_map.update key (Some unit) ops
    | Remove_operator op ->
      let _ok = validate_owner op in
      let key = (op.owner, op.operator, op.token_id) in
      Big_map.remove key ops
  in
  let next_operators = List.fold process_update updates store.operators in
  ([]: operation list), {store with operators = next_operators}

(*
  Mints tokens in batch to recipients
  Minters set a minimum price on behalf of recipient
*)
let mint_token (mints, store: mint list * storage): return =
  let process_mint (s, mint: storage * mint) =
    if Big_map.mem mint.token_id store.token_metadata then
      (failwith "token already exists" : storage)
    else
      let next_ledger = add_token (mint.to_, mint.token_id, s.ledger) in
      let metadata = {
        name = mint.name;
        tax = mint.tax;
        tax_interval = mint.tax_interval;
        tax_recipient = mint.tax_recipient;
        extras = mint.extras;
      } in
      let next_token_metadata = Big_map.update mint.token_id (Some metadata) s.token_metadata in
      let price = {
        current = mint.price;
        minimum = mint.price;
      } in
      let next_token_prices = Big_map.update (mint.to_, mint.token_id) (Some price) s.token_prices in
      let next_tax_records = start_tax (mint.to_, mint.token_id, next_token_prices, s.tax_records) in
      { s with
        ledger = next_ledger;
        token_metadata = next_token_metadata;
        token_prices = next_token_prices;
        tax_records = next_tax_records }
  in
  let next_store = List.fold process_mint mints store in
  ([]: operation list), next_store

(* Operator forces sales to buy tokens *)
let force_sale (sales, store: forced_sale list * storage): return =
  let process_sale (acc, tx: (operation list * storage * tez) * forced_sale) =
    List.fold
      (fun ((ops, s, amt), origin: (operation list * storage * tez) * forced_sale_origin) ->
        let key = origin.from_, origin.token_id in
        let price = find_price (key, s.token_prices) in
        if amt < price.current then
          (failwith "amount does not match cost" : operation list * storage * tez)
        else
          let _ok = is_operator (tx.to_, origin.token_id, s.operators) in
          let l = remove_token (origin.from_, origin.token_id, s.ledger) in
          let next_ledger = add_token (tx.to_, origin.token_id, l) in
          let r = end_tax (origin.from_, origin.token_id, s.token_prices, s.tax_records) in
          let next_price = {
            current = price.current + 1mutez;
            minimum = price.current + 1mutez;
          } in
          let next_token_prices = Big_map.update (tx.to_, origin.token_id) (Some next_price) s.token_prices in
          let next_tax_records = start_tax (tx.to_, origin.token_id, next_token_prices, r) in
          let next_store = {
            s with
            ledger = next_ledger;
            tax_records = next_tax_records;
            token_prices = next_token_prices;
          } in
          let next_amt = amt - price.current in
          let op =
            Tezos.transaction () price.current
            ( match (Tezos.get_contract_opt origin.from_ : unit contract option) with
              | Some contract -> contract
              | None -> failwith "contract does not exist" : unit contract)
          in
          op :: ops, next_store, next_amt
      ) tx.txs acc
  in
  let ops, next_store, remaining = List.fold process_sale sales (([] : operation list), store, Tezos.amount) in
  if remaining > 0tez then
    (failwith "amount does not match cost" : return)
  else
    ops, next_store

(* Deposit tax for self or another *)
let deposit_tax (param, store: tax_param * storage): return =
  if Tezos.amount <> param.amount then
    (failwith "amount does not match tax deposit" : return)
  else
    let deposit = find_tax_deposit (param.owner, store.tax_deposits) in
    let next_deposit = deposit + param.amount in
    let next_tax_deposits = Big_map.update param.owner (Some next_deposit) store.tax_deposits in
    let next_store = {store with tax_deposits = next_tax_deposits} in
    ([]: operation list), next_store

(* Operator can withdraw any unspent tax deposit *)
let withdraw_tax (param, store: tax_param * storage): return =
  if Tezos.sender <> param.owner then
    (failwith "FA2_NOT_OPERATOR" : return)
  else
    let _ok = is_operator (param.owner, param.token_id, store.operators) in
    match Big_map.find_opt param.owner store.tax_deposits with
    | None -> (failwith "tax deposit not found" : return)
    | Some deposit ->
      let spent_tax = calculate_spent_tax (param.owner, param.token_id, store) in
      let next_deposit =
        if param.amount <= deposit - spent_tax then
          deposit - param.amount
        else
          (failwith "not enough unspent tax to withdraw" : tez)
      in
      let next_tax_deposits = Big_map.update param.owner (Some next_deposit) store.tax_deposits in
      let next_store = {store with tax_deposits = next_tax_deposits} in
      let op =
        Tezos.transaction () param.amount
        ( match (Tezos.get_contract_opt param.owner : unit contract option) with
          | Some contract -> contract
          | None -> failwith "contract does not exist" : unit contract)
      in
      [op], next_store

(* Operator of token can claim spent taxes *)
let claim_tax (claims, store: tax_claim list * storage): return =
  let process_claim (acc, tx: return * tax_claim) =
    let contract =
      match (Tezos.get_contract_opt tx.to_ : unit contract option) with
      | Some contract -> contract
      | None -> (failwith "contract does not exist" : unit contract)
    in
    List.fold
      (fun ((ops, s), claim: return * tax_claim_origin) ->
        let metadata = find_metadata (claim.token_id, s.token_metadata) in
        let _ok = is_operator (metadata.tax_recipient, claim.token_id, s.operators) in
        let spent_tax = calculate_spent_tax (claim.from_, claim.token_id, s) in
        if spent_tax < claim.amount then
          (failwith "not enough tax spent to claim" : return)
        else
          let key = claim.from_, claim.token_id in
          let claimed = find_claimed (key, s.tax_claims) in
          let next_claimed = claimed + claim.amount in
          let next_tax_claims = Big_map.update (claim.from_, claim.token_id) (Some next_claimed) s.tax_claims in
          let next_store = {s with tax_claims = next_tax_claims} in
          let op = Tezos.transaction () claim.amount contract in
          op :: ops, next_store
      ) tx.txs acc
  in
  List.fold process_claim claims (([] : operation list), store)

(* Operator updates prices of tokens *)
let update_price (updates, store: price_update list * storage): return =
  let process_update (s, tx: storage * price_update) =
    List.fold
      (fun (s, update: storage * price_set) ->
        let _ok = is_operator (tx.owner, update.token_id, s.operators) in
        let tokens = find_tokens (tx.owner, s.ledger) in
        let key = tx.owner, update.token_id in
        let price = find_price (key, s.token_prices) in
        if not Set.mem update.token_id tokens then
          (failwith "token not owned" : storage)
        else if update.price < price.minimum then
          (failwith "cannot set below minimum price" : storage)
        else
          let r = end_tax (tx.owner, update.token_id, s.token_prices, s.tax_records) in
          let next_price = {price with current = update.price} in
          let next_token_prices = Big_map.update key (Some next_price) s.token_prices in
          let next_tax_records = start_tax (tx.owner, update.token_id, next_token_prices, r) in
          {s with token_prices = next_token_prices; tax_records = next_tax_records}
      ) tx.txs s
  in
  let next_store = List.fold process_update updates store in
  ([]: operation list), next_store

(* Main function *)

let main (action, store: entry_points * storage): return =
  match action with
  | Transfer transfers -> transfer (transfers, store)
  | Balance_of param -> balances (param, store)
  | Update_operators updates -> update_operators (updates, store)
  | Mint_token mints -> mint_token (mints, store)
  | Force_sale sales -> force_sale (sales, store)
  | Deposit_tax param -> deposit_tax (param, store)
  | Withdraw_tax param -> withdraw_tax (param, store)
  | Claim_tax claims -> claim_tax (claims, store)
  | Update_price updates -> update_price (updates, store)
  
(* Unit tests *)

let initial_storage: storage = {
  ledger = (Big_map.empty : ledger);
  operators = (Big_map.empty : operator_storage);
  token_metadata = (Big_map.empty : token_metadata_storage);
  token_prices = (Big_map.empty : token_price_storage);
  tax_deposits = (Big_map.empty : tax_deposit_storage);
  tax_records = (Big_map.empty : tax_record_storage);
  tax_claims = (Big_map.empty : tax_claim_storage);
}

let mint_test_token (to_, tax_recipient, c: address * address * entry_points contract) =
  let mints = [
    { to_ = to_;
      token_id = 0n;
      name = "My NFT";
      tax = 500n;
      tax_interval = 604800n;
      tax_recipient = tax_recipient;
      extras = (Map.empty : (string, string) map);
      price = 5tez };
  ] in
  let () = Test.set_source tax_recipient in
  Test.transfer_to_contract_exn c (Mint_token mints) 0tez

let find_record (key, tax_records: (address * token_id) * tax_record_storage): tax_record =
  let record_list = find_records (key, tax_records) in
  match List.head_opt record_list with
  | None -> (failwith "no records" : tax_record)
  | Some record -> record

let test_storage =
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let storage = Test.get_storage taddr in
  let example_addr = Test.nth_bootstrap_account 1 in
  let () = assert (Big_map.find_opt (example_addr, example_addr, 0n) storage.operators = (None : unit option)) in
  let () = assert (Big_map.find_opt example_addr storage.tax_deposits = (None : tez option)) in
  let () = assert (Big_map.find_opt (example_addr, 0n) storage.tax_claims = (None : tez option)) in
  ()

let test_mint_and_transfer =
  let () = Test.reset_state 4n ([] : tez list) in
  let first_owner_addr = Test.nth_bootstrap_account 1 in
  let tax_recipient_addr = Test.nth_bootstrap_account 2 in
  let second_owner_addr = Test.nth_bootstrap_account 3 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let c = Test.to_contract taddr in
  let () = mint_test_token (first_owner_addr, tax_recipient_addr, c) in
  let storage = Test.get_storage taddr in
  let first_tokens = find_tokens (first_owner_addr, storage.ledger) in
  let second_tokens = find_tokens (second_owner_addr, storage.ledger) in
  let () = assert (Set.mem 0n first_tokens) in
  let () = assert (Set.mem 0n second_tokens = false) in
  let metadata = find_metadata (0n, storage.token_metadata) in
  let () = assert (metadata.tax = 500n && metadata.tax_interval = 604800n) in
  let price = find_price ((first_owner_addr, 0n), storage.token_prices) in
  let () = assert (price.current = 5tez && price.minimum = 5tez) in
  let record_list = find_records ((first_owner_addr, 0n), storage.tax_records) in
  let record =
    match List.head_opt record_list with
    | None -> (failwith "no records" : tax_record)
    | Some record -> record
  in
  let () = assert (record.value = 5tez && record.end_date = (None : timestamp option)) in
  let transfers = [
    { from_ = first_owner_addr;
      txs = [
        { to_ = second_owner_addr;
          token_id = 0n;
          amount = 1n };
      ] };
  ] in
  let () = Test.set_source first_owner_addr in
  let () = Test.transfer_to_contract_exn c (Transfer transfers) 0tez in
  let storage = Test.get_storage taddr in
  let first_tokens = find_tokens (first_owner_addr, storage.ledger) in
  let second_tokens = find_tokens (second_owner_addr, storage.ledger) in
  let () = assert (Set.mem 0n second_tokens) in
  let () = assert (Set.mem 0n first_tokens = false) in
  let price = find_price ((second_owner_addr, 0n), storage.token_prices) in
  let () = assert (price.current = 5tez && price.minimum = 5tez) in
  let record = find_record ((first_owner_addr, 0n), storage.tax_records) in
  let () = assert (record.value = 5tez && record.end_date <> (None : timestamp option)) in
  let record = find_record ((second_owner_addr, 0n), storage.tax_records) in
  let () = assert (record.value = 5tez && record.end_date = (None : timestamp option)) in
  ()

let test_transfer_as_operator =
  let () = Test.reset_state 4n ([] : tez list) in
  let first_owner_addr = Test.nth_bootstrap_account 1 in
  let tax_recipient_addr = Test.nth_bootstrap_account 2 in
  let second_owner_addr = Test.nth_bootstrap_account 3 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let c = Test.to_contract taddr in
  let () = mint_test_token (first_owner_addr, tax_recipient_addr, c) in
  let updates = [
    Add_operator {
      owner = first_owner_addr;
      operator = second_owner_addr;
      token_id = 0n;
    }
  ] in
  let () = Test.set_source first_owner_addr in
  let () = Test.transfer_to_contract_exn c (Update_operators updates) 0tez in
  let transfers = [
    { from_ = first_owner_addr;
      txs = [
        { to_ = second_owner_addr;
          token_id = 0n;
          amount = 1n };
      ] };
  ] in
  let () = Test.set_source second_owner_addr in
  let () = Test.transfer_to_contract_exn c (Transfer transfers) 0tez in
  let storage = Test.get_storage taddr in
  let first_tokens = find_tokens (first_owner_addr, storage.ledger) in
  let second_tokens = find_tokens (second_owner_addr, storage.ledger) in
  let () = assert (Set.mem 0n second_tokens) in
  let () = assert (Set.mem 0n first_tokens = false) in
  ()

let test_force_sale =
  let () = Test.reset_state 4n ([] : tez list) in
  let first_owner_addr = Test.nth_bootstrap_account 1 in
  let tax_recipient_addr = Test.nth_bootstrap_account 2 in
  let second_owner_addr = Test.nth_bootstrap_account 3 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let c = Test.to_contract taddr in
  let () = mint_test_token (first_owner_addr, tax_recipient_addr, c) in
  let sales = [
    { to_ = second_owner_addr;
      txs = [
        { from_ = first_owner_addr;
          token_id = 0n };
      ] };
  ] in
  let () = Test.set_source second_owner_addr in
  let () = Test.transfer_to_contract_exn c (Force_sale sales) 5tez in
  let storage = Test.get_storage taddr in
  let first_tokens = find_tokens (first_owner_addr, storage.ledger) in
  let second_tokens = find_tokens (second_owner_addr, storage.ledger) in
  let () = assert (Set.mem 0n second_tokens) in
  let () = assert (Set.mem 0n first_tokens = false) in
  let price = find_price ((second_owner_addr, 0n), storage.token_prices) in
  let () = assert (price.current = 5.000001tez && price.minimum = 5.000001tez) in
  ()

let test_deposit_and_withdraw_tax =
  let () = Test.reset_state 3n ([] : tez list) in
  let owner_addr = Test.nth_bootstrap_account 1 in
  let tax_recipient_addr = Test.nth_bootstrap_account 2 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let c = Test.to_contract taddr in
  let () = mint_test_token (owner_addr, tax_recipient_addr, c) in
  let param = {
    owner = owner_addr;
    token_id = 0n;
    amount = 12tez;
  } in
  let () = Test.set_source owner_addr in
  let () = Test.transfer_to_contract_exn c (Deposit_tax param) 12tez in
  let storage = Test.get_storage taddr in
  let deposit = find_tax_deposit (owner_addr, storage.tax_deposits) in
  let () = assert (deposit = 12tez) in
  let param = {
    owner = owner_addr;
    token_id = 0n;
    amount = 7.5tez;
  } in
  let () = Test.transfer_to_contract_exn c (Withdraw_tax param) 0tez in
  let storage = Test.get_storage taddr in
  let deposit = find_tax_deposit (owner_addr, storage.tax_deposits) in
  let () = assert (deposit = 4.5tez) in
  ()

let test_claim_tax =
  let () = Test.reset_state 3n ([] : tez list) in
  let owner_addr = Test.nth_bootstrap_account 1 in
  let tax_recipient_addr = Test.nth_bootstrap_account 2 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let c = Test.to_contract taddr in
  let () = mint_test_token (owner_addr, tax_recipient_addr, c) in
  let param = {
    owner = owner_addr;
    token_id = 0n;
    amount = 12tez;
  } in
  let () = Test.set_source owner_addr in
  let () = Test.transfer_to_contract_exn c (Deposit_tax param) 12tez in
  let storage = Test.get_storage taddr in
  let claims = [
    { to_ = tax_recipient_addr;
      txs = [
        { from_ = owner_addr;
          token_id = 0n;
          amount = 0.00001tez };
      ] };
  ] in
  let () = Test.set_source tax_recipient_addr in
  let () = Test.transfer_to_contract_exn c (Claim_tax claims) 0tez in
  ()

let test_update_price =
  let () = Test.reset_state 3n ([] : tez list) in
  let owner_addr = Test.nth_bootstrap_account 1 in
  let tax_recipient_addr = Test.nth_bootstrap_account 2 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let c = Test.to_contract taddr in
  let () = mint_test_token (owner_addr, tax_recipient_addr, c) in
  let updates = [
    { owner = owner_addr;
      txs = [
        { token_id = 0n;
          price = 7tez };
      ] };
  ] in
  let () = Test.set_source owner_addr in
  let () = Test.transfer_to_contract_exn c (Update_price updates) 0tez in
  let storage = Test.get_storage taddr in
  let price = find_price ((owner_addr, 0n), storage.token_prices) in
  let () = assert (price.current = 7tez && price.minimum = 5tez) in
  ()
