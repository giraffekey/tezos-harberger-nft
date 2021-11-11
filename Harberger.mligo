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

type tax_record = {
  value: tez;
  amount: nat;
  start_date: timestamp;
  end_date: timestamp option;
}

type token_metadata = {
  token_id: token_id;
  owner: address;
  symbol: string;
  name: string;
  decimals: nat;
  tax: nat;
  extras: (string, string) map;
}

type create_token_param =
[@layout:comb]
{
  metadata: token_metadata;
  supply: nat;
}

type mint = transfer_destination

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

type token_price_storage = ((address * token_id), tez) big_map

type tax_deposit_storage = ((address * token_id), tez) big_map

type tax_record_storage = ((address * token_id), tax_record list) big_map

type storage = {
  ledger: ledger;
  operators: operator_storage;
  token_total_supply: token_total_supply;
  token_metadata: token_metadata_storage;
  token_prices: token_price_storage;
  tax_deposits: tax_deposit_storage;
  tax_records: tax_record_storage;
}

(* Helper functions *)

let find_balance (key, ledger: (address * nat) * ledger): nat =
  match Big_map.find_opt key ledger with
  | None -> 0n
  | Some bal -> bal

let find_records (key, tax_records: (address * nat) * tax_record_storage): tax_record list =
  match Big_map.find_opt key tax_records with
  | None -> []
  | Some record_list -> record_list

let find_price (key, token_prices: (address * nat) * token_price_storage): tez =
  match Big_map.find_opt key token_prices with
  | None -> 0tez
  | Some price -> price

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
    value = price;
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
        else if record.value = price && record.end_date = (None : timestamp option) then
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
  let make_transfer (s, tx: storage * transfer) =
    let token_exists (token_id: token_id): bool =
      if Big_map.mem token_id s.token_metadata
      then true
      else (failwith "token undefined" : bool)
    in
    let is_operator (token_id: token_id): bool =
      if Tezos.sender = tx.from_
      then true
      else if Big_map.mem (tx.from_, Tezos.sender, token_id) s.operators
      then true
      else (failwith "FA2_NOT_OPERATOR" : bool)
    in
    List.fold
      (fun (s, dest: storage * transfer_destination) ->
        let ok = token_exists dest.token_id && is_operator dest.token_id in
        let l = dec_balance (tx.from_, dest.token_id, dest.amount, s.ledger) in
        let ll = inc_balance (dest.to_, dest.token_id, dest.amount, l) in
        let r = end_tax (tx.from_, dest.token_id, dest.amount, s.token_prices, s.tax_records) in
        let rr = start_tax (dest.to_, dest.token_id, dest.amount, s.token_prices, r) in
        {store with ledger = ll; tax_records = rr}
      ) tx.txs store
  in
  let next_store = List.fold make_transfer transfers store in
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

(* Operator of token spec mints in batch to recipients *)
let mint_token (mints, store: mint list * storage): return =
  let process_mint (s, mint: storage * mint) =
    let is_operator (token_id: token_id): bool =
      match Big_map.find_opt token_id s.token_metadata with
      | None -> (failwith "token undefined" : bool)
      | Some metadata ->
        if Tezos.sender = metadata.owner
        then true
        else if Big_map.mem (metadata.owner, Tezos.sender, token_id) s.operators
        then true
        else (failwith "FA2_NOT_OPERATOR" : bool)
    in
    let ok = is_operator mint.token_id in
    let next_ledger = inc_balance (mint.to_, mint.token_id, mint.amount, s.ledger) in
    let next_tax_records = start_tax (mint.to_, mint.token_id, mint.amount, s.token_prices, s.tax_records) in
    {store with ledger = next_ledger; tax_records = next_tax_records}
  in
  let next_store = List.fold process_mint mints store in
  ([]: operation list), next_store

let force_sale (sales, store: forced_sale list * storage): return =
  ([]: operation list), store

let deposit_tax (param, store: tax_param * storage): return =
  ([]: operation list), store

let withdraw_tax (param, store: tax_param * storage): return =
  ([]: operation list), store

let claim_tax (claims, store: tax_claim list * storage): return =
  ([]: operation list), store

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
