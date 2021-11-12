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

type token_metadata = {
  token_id: token_id;
  owner: address;
  symbol: string;
  name: string;
  decimals: nat;
  tax: nat;
  tax_interval: nat;
  extras: (string, string) map;
}

type price = {
  current: tez;
  minimum: tez;
}

type tax_record = {
  value: tez;
  amount: nat;
  start_date: timestamp;
  end_date: timestamp option;
}

type create_token_param =
[@layout:comb]
{
  metadata: token_metadata;
  supply: nat;
}

type mint =
[@layout:comb]
{
  to_: address;
  token_id: token_id;
  amount: nat;
  price: tez;
  tax_deposit: tez;
}

type forced_sale_origin =
[@layout:comb]
{
  from_: address;
  token_id: token_id;
  amount: nat;
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
  from_: address;
  txs: price_set list;
}

(* Storage *)

type ledger = ((address * token_id), nat) big_map

type operator_storage = ((address * address * token_id), unit) big_map

type token_total_supply = (token_id, nat) big_map

type token_metadata_storage = (token_id, token_metadata) big_map

type token_price_storage = ((address * token_id), price) big_map

type tax_deposit_storage = ((address * token_id), tez) big_map

type tax_record_storage = ((address * token_id), tax_record list) big_map

type tax_claim_storage = ((address * token_id), tez) big_map

type storage = {
  ledger: ledger;
  operators: operator_storage;
  token_total_supply: token_total_supply;
  token_metadata: token_metadata_storage;
  token_prices: token_price_storage;
  tax_deposits: tax_deposit_storage;
  tax_records: tax_record_storage;
  tax_claims: tax_claim_storage;
}

(* Helper functions *)

let find_balance (key, ledger: (address * token_id) * ledger): nat =
  match Big_map.find_opt key ledger with
  | None -> 0n
  | Some bal -> bal

let find_records (key, tax_records: (address * token_id) * tax_record_storage): tax_record list =
  match Big_map.find_opt key tax_records with
  | None -> []
  | Some record_list -> record_list

let find_price (key, token_prices: (address * token_id) * token_price_storage): price =
  match Big_map.find_opt key token_prices with
  | None -> {
      current = 0tez;
      minimum = 0tez;
    }
  | Some price -> price

let find_metadata (token_id, token_metadata: token_id * token_metadata_storage): token_metadata =
  match Big_map.find_opt token_id token_metadata with
  | None -> (failwith "token undefined" : token_metadata)
  | Some metadata -> metadata

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

let inc_balance (owner, token_id, amt, ledger
    : address * token_id * nat * ledger): ledger =
  let key = owner, token_id in
  let bal = find_balance (key, ledger) in
  let next_bal = bal + amt in
  if next_bal = 0n
  then Big_map.remove key ledger
  else Big_map.update key (Some next_bal) ledger

let dec_balance (owner, token_id, amt, ledger
    : address * token_id * nat * ledger): ledger =
  let key = owner, token_id in
  let bal = find_balance (key, ledger) in
  match Michelson.is_nat (bal - amt) with
  | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : ledger)
  | Some next_bal ->
    if next_bal = 0n
    then Big_map.remove key ledger
    else Big_map.update key (Some next_bal) ledger

let start_tax (owner, token_id, amt, token_prices, tax_records
    : address * token_id * nat * token_price_storage * tax_record_storage): tax_record_storage =
  let key = owner, token_id in
  let record_list = find_records (key, tax_records) in
  let price = find_price (key, token_prices) in
  let record = {
    value = price.current;
    amount = amt;
    start_date = Tezos.now;
    end_date = (None : timestamp option);
  } in
  let next_record_list = record :: record_list in
  Big_map.update key (Some next_record_list) tax_records

let end_tax (owner, token_id, amt, token_prices, tax_records
    : address * token_id * nat * token_price_storage * tax_record_storage): tax_record_storage =
  let key = owner, token_id in
  let record_list = find_records (key, tax_records) in
  let price = find_price (key, token_prices) in
  let next_record_list, remainder =
    List.fold
      (fun ((l, rem), record: (tax_record list * nat) * tax_record) ->
        if rem = 0n then
          record :: l, 0n
        else if record.value = price.current && record.end_date = (None : timestamp option) then
          if record.amount > rem then
            let end_record = {
              value = record.value;
              amount = rem;
              start_date = record.start_date;
              end_date = Some Tezos.now;
            } in
            let next_record = {
              value = record.value;
              amount = abs (record.amount - rem);
              start_date = Tezos.now;
              end_date = (None : timestamp option);
            } in
            next_record :: (end_record :: l), 0n
          else
            let end_record = {
              value = record.value;
              amount = record.amount;
              start_date = record.start_date;
              end_date = Some Tezos.now;
            } in
            end_record :: l, abs (rem - record.amount)
        else
          record :: l, rem
      ) record_list (([] : tax_record list), amt)
  in
  if remainder = 0n then
    Big_map.update key (Some next_record_list) tax_records
  else
    (failwith "FA2_INSUFFICIENT_BALANCE" : tax_record_storage)

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
        let added = tax_amount * time_passed / metadata.tax_interval * record.amount in
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
| Create_token of create_token_param
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
        let ok = is_operator (dest.to_, dest.token_id, s.operators) in
        let l = dec_balance (tx.from_, dest.token_id, dest.amount, s.ledger) in
        let next_ledger = inc_balance (dest.to_, dest.token_id, dest.amount, l) in
        let r = end_tax (tx.from_, dest.token_id, dest.amount, s.token_prices, s.tax_records) in
        let next_tax_records = start_tax (dest.to_, dest.token_id, dest.amount, s.token_prices, r) in
        {s with ledger = next_ledger; tax_records = next_tax_records}
      ) tx.txs s
  in
  let next_store = List.fold process_transfer transfers store in
  ([]: operation list), next_store

(* Send balances to callback in batches *)
let balances (param, store: balance_of_param * storage): return =
  let to_balance (r: balance_of_request) =
    if not Big_map.mem r.token_id store.token_metadata
    then (failwith "FA2_TOKEN_UNDEFINED" : balance_of_response)
    else
      let key = r.owner, r.token_id in
      let bal = find_balance (key, store.ledger) in
      { request = r; balance = bal }
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
      let ok = validate_owner op in
      let key = (op.owner, op.operator, op.token_id) in
      Big_map.update key (Some unit) store.operators
    | Remove_operator op ->
      let ok = validate_owner op in
      let key = (op.owner, op.operator, op.token_id) in
      Big_map.remove key store.operators
  in
  let next_operators = List.fold process_update updates store.operators in
  ([]: operation list), {store with operators = next_operators}

(* Create new token specification with a total supply *)
let create_token (param, store: create_token_param * storage): return =
  let token_id = param.metadata.token_id in
  if Big_map.mem token_id store.token_metadata then
    (failwith "token already exists" : return)
  else
    let next_token_metadata = Big_map.update token_id (Some param.metadata) store.token_metadata in
    let next_token_total_supply = Big_map.update token_id (Some param.supply) store.token_total_supply in
    let next_store = {
      store with
      token_metadata = next_token_metadata;
      token_total_supply = next_token_total_supply;
    } in
    ([]: operation list), next_store

(*
  Operator of token spec mints in batch to recipients
  Operators set a default price on behalf of recipient and have the option of funding the tax deposit
*)
let mint_token (mints, store: mint list * storage): return =
  let process_mint ((s, amt), mint: (storage * tez) * mint) =
    if amt < mint.tax_deposit then
      (failwith "amount does not match tax deposit" : storage * tez)
    else
      let metadata = find_metadata (mint.token_id, s.token_metadata) in
      let ok = is_operator (metadata.owner, mint.token_id, s.operators) in
      let next_ledger = inc_balance (mint.to_, mint.token_id, mint.amount, s.ledger) in
      let price = {
        current = mint.price;
        minimum = mint.price;
      } in
      let next_token_prices = Big_map.update (mint.to_, mint.token_id) (Some price) s.token_prices in
      let next_tax_deposits = Big_map.update (mint.to_, mint.token_id) (Some mint.tax_deposit) s.tax_deposits in
      let next_tax_records = start_tax (mint.to_, mint.token_id, mint.amount, next_token_prices, s.tax_records) in
      let next_store = {
        s with
        ledger = next_ledger;
        token_prices = next_token_prices;
        tax_deposits = next_tax_deposits;
        tax_records = next_tax_records;
      } in
      let next_amt = amt - mint.tax_deposit in
      (next_store, next_amt)
  in
  let next_store, remaining = List.fold process_mint mints (store, Tezos.amount) in
  if remaining > 0tez then
    (failwith "amount does not match tax deposit" : return)
  else
    ([]: operation list), next_store

let force_sale (sales, store: forced_sale list * storage): return =
  let process_sale (acc, tx: (operation list * storage * tez) * forced_sale) =
    List.fold
      (fun ((ops, s, amt), origin: (operation list * storage * tez) * forced_sale_origin) ->
        let key = origin.from_, origin.token_id in
        let price = find_price (key, s.token_prices) in
        let cost = price.current * origin.amount in
        if amt < cost then
          (failwith "amount does not match cost" : operation list * storage * tez)
        else
          let ok = is_operator (tx.to_, origin.token_id, s.operators) in
          let l = dec_balance (origin.from_, origin.token_id, origin.amount, s.ledger) in
          let next_ledger = inc_balance (tx.to_, origin.token_id, origin.amount, l) in
          let r = end_tax (origin.from_, origin.token_id, origin.amount, s.token_prices, s.tax_records) in
          let next_price = {
            current = price.current + 1mutez;
            minimum = price.current + 1mutez;
          } in
          let next_token_prices = Big_map.update (tx.to_, origin.token_id) (Some next_price) s.token_prices in
          let next_tax_records = start_tax (tx.to_, origin.token_id, origin.amount, next_token_prices, r) in
          let next_store = {
            s with
            ledger = next_ledger;
            tax_records = next_tax_records;
            token_prices = next_token_prices;
          } in
          let next_amt = amt - cost in
          let op =
            Tezos.transaction () cost
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
    let next_tax_deposits = Big_map.update (param.owner, param.token_id) (Some param.amount) store.tax_deposits in
    let next_store = {store with tax_deposits = next_tax_deposits} in
    ([]: operation list), next_store

(* Operator can withdraw any unspent tax deposit *)
let withdraw_tax (param, store: tax_param * storage): return =
  if Tezos.sender <> param.owner then
    (failwith "FA2_NOT_OPERATOR" : return)
  else
    let ok = is_operator (param.owner, param.token_id, store.operators) in
    match Big_map.find_opt (param.owner, param.token_id) store.tax_deposits with
    | None -> (failwith "tax deposit not found" : return)
    | Some deposit ->
      let spent_tax = calculate_spent_tax (param.owner, param.token_id, store) in
      let next_deposit =
        if param.amount >= deposit - spent_tax then
          deposit - param.amount
        else
          (failwith "not enough unspent tax to withdraw" : tez)
      in
      let next_tax_deposits = Big_map.update (param.owner, param.token_id) (Some next_deposit) store.tax_deposits in
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
        let ok = is_operator (metadata.owner, claim.token_id, s.operators) in
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

let update_price (updates, store: price_update list * storage): return =
  ([]: operation list), store

(* Main function *)

let main (action, store: entry_points * storage): return =
  match action with
  | Transfer transfers -> transfer (transfers, store)
  | Balance_of param -> balances (param, store)
  | Update_operators updates -> update_operators (updates, store)
  | Create_token param -> create_token (param, store)
  | Mint_token mints -> mint_token (mints, store)
  | Force_sale sales -> force_sale (sales, store)
  | Deposit_tax param -> deposit_tax (param, store)
  | Withdraw_tax param -> withdraw_tax (param, store)
  | Claim_tax claims -> claim_tax (claims, store)
  | Update_price updates -> update_price (updates, store)
  
(* Unit tests *)
