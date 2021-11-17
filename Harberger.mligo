#include "FA2Interface.mligo"
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
  next_price: tez;
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
  (* Percent with 2 decimal places, i.e. 550n = 5.5% *)
  tax: nat;
  (* In seconds *)
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

(* Check if current sender has been added as operator to token by owner *)
let is_operator (owner, token_id, operators: address * token_id * operator_storage): bool =
  if Tezos.sender = owner
  then true
  else if Big_map.mem (owner, Tezos.sender, token_id) operators
  then true
  else (failwith "FA2_NOT_OPERATOR" : bool)

(* Add token id to owner's collection *)
let add_token (owner, token_id, ledger: address * token_id * ledger): ledger =
  let tokens = find_tokens (owner, ledger) in
  let next_tokens = Set.add token_id tokens in
  Big_map.update owner (Some next_tokens) ledger

(* Remove token id from owner's collection *)
let remove_token (owner, token_id, ledger: address * token_id * ledger): ledger =
  let tokens = find_tokens (owner, ledger) in
  if Set.mem token_id tokens then
    let next_tokens = Set.remove token_id tokens in
    if Set.size next_tokens = 0n
    then Big_map.remove owner ledger
    else Big_map.update owner (Some next_tokens) ledger
  else
    (failwith "FA2_INSUFFICIENT_BALANCE" : ledger)

(* Begin taxation period at current price and time with no end date *)
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

(* Find current tax record for a token and end it, preventing further charges *)
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

(* Calculate how much tax is owed *)
let calculate_spent_tax (owner, token_id, store: address * token_id * storage): tez =
  let key = owner, token_id in
  let record_list = find_records (key, store.tax_records) in
  let metadata = find_metadata (token_id, store.token_metadata) in
  let spent =
    List.fold
      (fun (spent, record: tez * tax_record) ->
        (* If tax is still ongoing, use current time *)
        let end_date =
          match record.end_date with
          | None -> Tezos.now
          | Some date -> date
        in
        let time_passed = abs (end_date - record.start_date) in
        (* 550n would be a 5.5% tax, so divide by 1000n *)
        let tax_amount = record.value * metadata.tax / 1000n in
        (* Time passed and tax interval are both in seconds *)
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
          (* Move token from "tx.from_" to "dest.to_" *)
          let l = remove_token (tx.from_, dest.token_id, s.ledger) in
          let next_ledger = add_token (dest.to_, dest.token_id, l) in
          (* Previous owner should not be taxed on a token they no longer own *)
          let r = end_tax (tx.from_, dest.token_id, s.token_prices, s.tax_records) in
          let price = find_price ((tx.from_, dest.token_id), s.token_prices) in
          (* Current and minimum price are transferred along with token *)
          let next_token_prices = Big_map.update (dest.to_, dest.token_id) (Some price) s.token_prices in
          (*
            This is currently a security vulnerability
            Transferring a token to someone forces them to pay a tax on it
            A request/accept system would allow transfers to be held in escrow instead
          *)
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
      (* Tokens are non-fungible, so balance can only be 0 or 1 *)
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

(* Mints tokens in batch to recipients *)
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
      (* Minters set a current and minimum price on behalf of recipient *)
      let price = {
        current = mint.price;
        minimum = mint.price;
      } in
      let next_token_prices = Big_map.update (mint.to_, mint.token_id) (Some price) s.token_prices in
      (*
        This is currently a security vulnerability
        It allows anyone to mint a token to someone and force them to pay them a tax on it
        This can be used maliciously to steal tax deposits
      *)
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
        (* Price must always be greater than what it was bought at *)
        let next_minimum = price.current + 1mutez in
        if amt < price.current then
          (failwith "amount does not match cost" : operation list * storage * tez)
        else if origin.next_price < next_minimum then
          (failwith "new price is too low" : operation list * storage * tez)
        else
          let _ok = is_operator (tx.to_, origin.token_id, s.operators) in
          let l = remove_token (origin.from_, origin.token_id, s.ledger) in
          let next_ledger = add_token (tx.to_, origin.token_id, l) in
          let r = end_tax (origin.from_, origin.token_id, s.token_prices, s.tax_records) in
          let next_price = {
            current = origin.next_price;
            minimum = next_minimum;
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
          let deposit = find_tax_deposit (claim.from_, store.tax_deposits) in
          let next_deposit = deposit - claim.amount in
          let next_tax_deposits = Big_map.update claim.from_ (Some next_deposit) store.tax_deposits in
          let next_store = {s with tax_claims = next_tax_claims; tax_deposits = next_tax_deposits} in
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
          (* Tax records need to be updated so it is taxing on correct price *)
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
  