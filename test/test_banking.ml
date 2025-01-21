open OUnit2

let make_debt_val_test name expected_output curr_balance amount = 
  name >:: (fun _ -> assert_equal expected_output (Account.debt_val curr_balance amount))


let tests = "test suite for debt operation" >::: [
  make_debt_val_test "same-currency" (Account.Amount(Real, 900.)) (Account.Amount(Account.Real, 1000.))  (Account.Amount(Account.Real, 900.));
  make_debt_val_test "different-currency" (Account.Amount(Real, 6.4375)) (Account.Amount(Account.Real, 1000.))  (Account.Amount(Account.Euro, 1.));
  make_debt_val_test "invalid-debt-same-currency" (Account.Amount(Real, 0.)) (Account.Amount(Account.Real, 1000.))  (Account.Amount(Account.Real, 100000.));
  make_debt_val_test "invalid-debt-diff-currency" (Account.Amount(Real, 0.)) (Account.Amount(Account.Real, 1000.))  (Account.Amount(Account.Euro, 1000.));
]

let _ = run_test_tt_main tests