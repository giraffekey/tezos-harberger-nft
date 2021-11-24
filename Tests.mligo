#include "Harberger.mligo"

(* Unit tests *)

let initial_storage: storage = {
  ledger = (Big_map.empty : ledger);
  operators = (Big_map.empty : operator_storage);
  token_metadata = (Big_map.empty : token_metadata_storage);
  token_prices = (Big_map.empty : token_price_storage);
  transfer_requests = (Big_map.empty : transfer_request_storage);
  tax_deposits = (Big_map.empty : tax_deposit_storage);
  tax_records = (Big_map.empty : tax_record_storage);
  tax_claims = (Big_map.empty : tax_claim_storage);
}

let mint_test_token (to_, tax_recipient, c: address * address * entry_points contract) =
  let mints = [
    { from_ = tax_recipient;
      txs = [
        { to_ = to_;
          token_id = 0n;
          name = "My NFT";
          tax = 500n;
          tax_interval = 604800n;
          tax_recipient = tax_recipient;
          extras = (Map.empty : (string, string) map);
          price = 5tez };
      ] };
  ] in
  let () = Test.set_source tax_recipient in
  let () = Test.transfer_to_contract_exn c (Mint_token mints) 0tez in
  let accepts = [
    { to_ = to_;
      txs = [{ token_id = 0n; price = 5tez }] };
  ] in
  let () = Test.set_source to_ in
  let () = Test.transfer_to_contract_exn c (Accept_request accepts) 0tez in
  ()

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
  let () = Test.transfer_to_contract_exn c (Transfer transfers) 0tez in
  let accepts = [
    { to_ = second_owner_addr;
      txs = [{ token_id = 0n; price = 10tez }] };
  ] in
  let () = Test.set_source second_owner_addr in
  let () = Test.transfer_to_contract_exn c (Accept_request accepts) 0tez in
  let storage = Test.get_storage taddr in
  let first_tokens = find_tokens (first_owner_addr, storage.ledger) in
  let second_tokens = find_tokens (second_owner_addr, storage.ledger) in
  let () = assert (Set.mem 0n second_tokens) in
  let () = assert (Set.mem 0n first_tokens = false) in
  let price = find_price ((second_owner_addr, 0n), storage.token_prices) in
  let () = assert (price.current = 10tez && price.minimum = 5tez) in
  let record = find_record ((first_owner_addr, 0n), storage.tax_records) in
  let () = assert (record.value = 5tez && record.end_date <> (None : timestamp option)) in
  let record = find_record ((second_owner_addr, 0n), storage.tax_records) in
  let () = assert (record.value = 10tez && record.end_date = (None : timestamp option)) in
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
  let accepts = [
    { to_ = second_owner_addr;
      txs = [{ token_id = 0n; price = 10tez }] };
  ] in
  let () = Test.set_source second_owner_addr in
  let () = Test.transfer_to_contract_exn c (Accept_request accepts) 0tez in
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
          token_id = 0n;
          next_price = 10tez };
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
  let () = assert (price.current = 10tez && price.minimum = 5.000001tez) in
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
