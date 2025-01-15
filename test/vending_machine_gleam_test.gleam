import gleam/io
import gleeunit
import gleeunit/should
import vending_machine_gleam.{Machine, init, user_event_handler}

pub fn main() {
  gleeunit.main()
}

pub fn user_event_handler_input_money_test() {
  // should take multiple inputs
  init()
  |> user_event_handler("1/100")
  |> user_event_handler("1/150")
  |> should.equal(Machine(..init(), payment: 250))
  // should not take non integer strings
  // init()
  // |> user_event_handler("1/woo")
  // |> should.equal(Machine(..init(), display: "Invalid Money Input"))
}
// pub fn user_event_handler_select_item_test() {
//   // should give back item and change if valid payment and drink available
//   init()
//   |> user_event_handler("1/100")
//   |> user_event_handler("1/150")
//   |> should.equal(Machine(..init(), payment: 250))

//   // should not take non integer strings
//   init()
//   |> user_event_handler("0/woo")
//   |> should.equal(Machine(..init(), display: "Invalid Money Input"))
// }
