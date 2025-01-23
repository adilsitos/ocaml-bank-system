type currency = 
  | Real 
  | Dollar 
  | Euro

type bankType = 
  | BancoDoBrasil
  | BancoSistema
  | CreditosBank

type currencyAmount = Amount of currency * float

type account = {
  id: float;
   name: string;
   balance: currencyAmount;
   bank: bankType;
   score: int; (*score will  be a random value that will define if an account is reliable or not*)
  }

let create_account currency bank = {
  id = Random.float 10000.;
  name = string_of_float (Random.float 10000.);
  balance = Amount(currency,Random.float 1000.);
  bank = bank;
  score = Random.int 10;
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


let debt_aux curr_balance sent_val = 
  let sent_val_dollar = convert_to_dollar sent_val in 
  let curr_balance_dollar = convert_to_dollar curr_balance in 
    match curr_balance with 
    | Amount(currency, _) -> 
      if curr_balance_dollar < sent_val_dollar then 
        Amount(currency, 0.)
    else
      let back_to_real = from_dollar_to_currency (Amount(currency, sent_val_dollar)) in
      Amount(currency, back_to_real)

let debt sender amount = 
  match sender with
  | account -> begin 
     let debt_res = debt_aux account.balance amount in 
     match debt_res with
     | Amount(_, value) -> 
      if value = 0. then 
        sender 
    else 
      {
        id = sender.id;
        name = sender.name;
        bank = sender.bank;
        balance = debt_res;
        score = sender.score;
      } 
  end

let credit_aux curr_balance received_val = 
  let received_val_dollar = convert_to_dollar received_val in 
  let curr_balance_dollar = convert_to_dollar curr_balance in 
  let new_amount = received_val_dollar +. curr_balance_dollar in 
  match curr_balance with
  | Amount(currency, _) -> 
    Amount(currency, from_dollar_to_currency (Amount(currency, new_amount)))  

let credit receiver amount = 
  match receiver with
  | account -> 
    let credit_res = credit_aux account.balance amount in 
    {
        id = account.id;
        name = account.name;
        bank = account.bank;
        balance = credit_res;
        score = account.score;
    } 

type bank = {
  name : bankType;
  id: float;
}

(*First validation: a transaction needs a bank, so we need to check it*) 
(*Second validation: create a simple fraud verification *)
(*Approach to test concurrency (using actor model): Each time a transaction is created, the 
  bank needs to create three actors: one to handle the transaction operation and check if everything went ok, and two
    others to maintain the state of the sender and receiver. If new transactions are made to these actors, the model
  needs to make sure that these new operations are applied 
*)

type transaction_response = 
  | Tallowed of account * account
  | Tdenied

let check_bank account = 
  match account.bank with
  | BancoDoBrasil -> true
  | BancoSistema -> true 
  | CreditosBank -> true

let allow_operation receiver = 
  match receiver with
  | account -> if account.score < 5 then 
    false else 
    true

let transaction_operation sender amount receiver = 
  if not (check_bank sender) && not (check_bank receiver) then 
    Tdenied
else if allow_operation receiver = false then 
    Tdenied
else 
  let new_sender = debt sender amount in 
  (*create  some validation here, this way, it is possible to 
insert random errors, and test how the concurrent models work*)
  let new_receiver = credit receiver amount in 
  Tallowed(new_sender, new_receiver) 




