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