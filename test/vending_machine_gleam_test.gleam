import gleeunit
import gleeunit/should
import vending_machine_gleam.{Item, Machine, init, update}

pub fn main() {
  gleeunit.main()
}

pub fn input_money_happy_path_test() {
  init()
  |> update("1/100")
  |> update("1/150")
  |> should.equal(Machine(..init(), payment: 250, display: "Total : 250"))
}

pub fn input_money_rejects_bad_money_input_test() {
  init()
  |> update("1/ten")
  |> should.equal(Machine(..init(), display: "Invalid Money Input"))
}

pub fn money_return_test() {
  init()
  |> update("2/")
  |> should.equal(Machine(..init(), display: "0 returned"))
}

pub fn item_select_with_money_test() {
  let test_item = Item(cost: 200, name: "test_item", stock: 1)
  let inital_machine = init()
  let expected =
    Machine(
      ..init(),
      display: "Here is a test_item. Your change is 100",
      items: [Item(..test_item, stock: 0)],
    )

  Machine(..inital_machine, items: [test_item])
  |> update("1/100")
  |> update("1/200")
  |> update("0/test_item")
  |> should.equal(expected)
}

pub fn item_select_with_not_enough_money_test() {
  let test_item = Item(cost: 301, name: "test_item", stock: 1)
  let inital_machine = init()
  let expected =
    Machine(..init(), payment: 300, display: "You require 1 more", items: [
      test_item,
    ])

  Machine(..inital_machine, items: [test_item])
  |> update("1/100")
  |> update("1/200")
  |> update("0/test_item")
  |> should.equal(expected)
}

pub fn item_select_out_of_stock_test() {
  let test_item = Item(cost: 300, name: "test_item", stock: 0)
  let inital_machine = init()
  let expected =
    Machine(
      ..init(),
      payment: 300,
      display: "Selected item out of stock",
      items: [test_item],
    )

  Machine(..inital_machine, items: [test_item])
  |> update("1/100")
  |> update("1/200")
  |> update("0/test_item")
  |> should.equal(expected)
}
