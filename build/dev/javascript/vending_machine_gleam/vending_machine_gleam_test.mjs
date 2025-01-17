import * as $gleeunit from "../gleeunit/gleeunit.mjs";
import * as $should from "../gleeunit/gleeunit/should.mjs";
import { toList } from "./gleam.mjs";
import * as $vending_machine_gleam from "./vending_machine_gleam.mjs";
import { Item, Machine, init, update } from "./vending_machine_gleam.mjs";

export function main() {
  return $gleeunit.main();
}

export function input_money_happy_path_test() {
  let _pipe = init();
  let _pipe$1 = update(_pipe, "1/100");
  let _pipe$2 = update(_pipe$1, "1/150");
  return $should.equal(
    _pipe$2,
    (() => {
      let _record = init();
      return new Machine(_record.bank, 250, "Total : 250", _record.items);
    })(),
  );
}

export function input_money_rejects_bad_money_input_test() {
  let _pipe = init();
  let _pipe$1 = update(_pipe, "1/ten");
  return $should.equal(
    _pipe$1,
    (() => {
      let _record = init();
      return new Machine(
        _record.bank,
        _record.payment,
        "Invalid Money Input",
        _record.items,
      );
    })(),
  );
}

export function money_return_test() {
  let _pipe = init();
  let _pipe$1 = update(_pipe, "2/");
  return $should.equal(
    _pipe$1,
    (() => {
      let _record = init();
      return new Machine(
        _record.bank,
        _record.payment,
        "0 returned",
        _record.items,
      );
    })(),
  );
}

export function item_select_with_money_test() {
  let test_item = new Item(200, "test_item", 1);
  let inital_machine = init();
  let expected = (() => {
    let _record = init();
    return new Machine(
      _record.bank,
      _record.payment,
      "Here is a test_item. Your change is 100",
      toList([
        (() => {
          let _record$1 = test_item;
          return new Item(_record$1.cost, _record$1.name, 0);
        })(),
      ]),
    );
  })();
  let _pipe = (() => {
    let _record = inital_machine;
    return new Machine(
      _record.bank,
      _record.payment,
      _record.display,
      toList([test_item]),
    );
  })();
  let _pipe$1 = update(_pipe, "1/100");
  let _pipe$2 = update(_pipe$1, "1/200");
  let _pipe$3 = update(_pipe$2, "0/test_item");
  return $should.equal(_pipe$3, expected);
}

export function item_select_with_not_enough_money_test() {
  let test_item = new Item(301, "test_item", 1);
  let inital_machine = init();
  let expected = (() => {
    let _record = init();
    return new Machine(
      _record.bank,
      300,
      "You require 1 more",
      toList([test_item]),
    );
  })();
  let _pipe = (() => {
    let _record = inital_machine;
    return new Machine(
      _record.bank,
      _record.payment,
      _record.display,
      toList([test_item]),
    );
  })();
  let _pipe$1 = update(_pipe, "1/100");
  let _pipe$2 = update(_pipe$1, "1/200");
  let _pipe$3 = update(_pipe$2, "0/test_item");
  return $should.equal(_pipe$3, expected);
}

export function item_select_out_of_stock_test() {
  let test_item = new Item(300, "test_item", 0);
  let inital_machine = init();
  let expected = (() => {
    let _record = init();
    return new Machine(
      _record.bank,
      300,
      "Selected item out of stock",
      toList([test_item]),
    );
  })();
  let _pipe = (() => {
    let _record = inital_machine;
    return new Machine(
      _record.bank,
      _record.payment,
      _record.display,
      toList([test_item]),
    );
  })();
  let _pipe$1 = update(_pipe, "1/100");
  let _pipe$2 = update(_pipe$1, "1/200");
  let _pipe$3 = update(_pipe$2, "0/test_item");
  return $should.equal(_pipe$3, expected);
}
