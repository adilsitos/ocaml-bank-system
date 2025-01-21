type currency = 
  | Real 
  | Dollar 
  | Euro

type bank = 
  | BB
  | SI
  | CR

type currencyAmount = Amount of currency * float

type account = {id: float; name: string; balance: currencyAmount; bank: bank  }

let create_account currency bank = {
  id = Random.float 10000.;
  name = string_of_float (Random.float 10000.);
  balance = Amount(currency,Random.float 1000.);
  bank = bank;
}

(*dollar conversion f*)
let conversions = [(Real, 0.16); (Euro, 1.03); (Dollar, 1.)]

let rec get_conversion to_curr lst = 
  match lst with
  | [] -> None 
  | (k', v') :: t -> if to_curr = k' then Some v' else get_conversion to_curr t  

let get_bank account = 
  account.bank

let get = function 
  | Some v -> v
  | None -> raise (Invalid_argument "option is None")

(*for example, sending real converted in euro*)

let convert_to_dollar amount = 
  match amount with
  | Amount(currency, value) ->
    let dollar_conversion = get (get_conversion currency conversions) in 
    value *. dollar_conversion

let from_dollar_to_currency amount = 
  match amount with
  | Amount(currency, value) -> 
    let dollar_conversion = get (get_conversion currency conversions) in 
    value /. dollar_conversion


(* let convert_balance sender amount currency = begin 
  let new_balance_converted_to_currency = calculate_amount_to_currency sender.balance currency in
  let sending_value = calculate_amount_to_currency amount currency in 
  if sending_value > new_balance_converted_to_currency then 
    0.
else 
  sending_value
end  *)

let debt_val curr_balance sent_val = 
  let sent_val_dollar = convert_to_dollar sent_val in 
  let curr_balance_dollar = convert_to_dollar curr_balance in 
    match curr_balance with 
    | Amount(currency, _) -> 
      if curr_balance_dollar < sent_val_dollar then 
        Amount(currency, 0.)
    else
      let back_to_real = from_dollar_to_currency (Amount(currency, sent_val_dollar)) in
      Amount(currency, back_to_real)
  

(* let debt sender amount = 
  let debt_val = debt_val sender.balance amount in 
  if debt_val = 0. then 
    sender else 
  match sender.balance with
  | Amount(currency, value) -> 
    let debt_curr = calculate_amount_to_currency amount currency in 
    {
      name = sender.name;
      bank = sender.bank;
      id = sender.id;
      balance = Amount(currency, value -. debt_curr)
    }   *)

let greet name = Printf.sprintf "Hello, %s!" name