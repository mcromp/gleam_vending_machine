import gleam/int
import gleam/result

import gleam/list
import gleam/string

type Money =
  Int

pub type Item {
  Item(cost: Money, name: String, stock: Int)
}

pub type UserEvent {
  SelectItem
  InputMoney
  MoneyReturn
}

pub type Machine {
  Machine(bank: Money, payment: Money, display: String, items: List(Item))
}

pub fn init() -> Machine {
  Machine(bank: 0, payment: 0, display: handle_display(Inital), items: [
    Item(cost: 150, name: "tea", stock: 10),
    Item(cost: 100, name: "orange soda", stock: 3),
    Item(cost: 200, name: "purple soda", stock: 20),
    Item(cost: 250, name: "water", stock: 10),
  ])
}

type DisplayStatus {
  ItemOutOfStock
  MoneyInputSuccess(amount: Money)
  ItemSelectSuccess(name: String, change: Money)
  NotEnoughMoney(amount: Money)
  NoItem
  MoneyReturnSuccess(amount: Money)
  InvalidMoney
  Inital
}

fn handle_display(s: DisplayStatus) -> String {
  case s {
    Inital -> "Hello! Awaiting Input"
    ItemOutOfStock -> "Selected item out of stock"
    ItemSelectSuccess(name, change) ->
      "Here is a " <> name <> ". Your change is " <> change |> int.to_string
    MoneyInputSuccess(amount) -> "Total : " <> amount |> int.to_string
    NoItem -> "Item not found"
    NotEnoughMoney(amount) ->
      "You require " <> amount |> int.to_string <> " more"
    MoneyReturnSuccess(amount) -> amount |> int.to_string <> " returned"
    InvalidMoney -> "Invalid Money Input"
  }
}

pub fn parse_user_event(e: String) -> Result(#(UserEvent, String), String) {
  let #(code, opt) =
    e
    |> string.split_once(on: "/")
    |> result.unwrap(#("", ""))
  case code {
    "0" -> Ok(#(SelectItem, opt))
    "1" -> Ok(#(InputMoney, opt))
    "2" -> Ok(#(MoneyReturn, opt))
    _ -> Error("Unrecognized User Event")
  }
}

pub fn handle_item_selection(ctx: Machine, item: Item) {
  case item.stock > 0, ctx.payment >= item.cost {
    False, _ -> Machine(..ctx, display: handle_display(ItemOutOfStock))
    _, False -> {
      let amount = item.cost - ctx.payment
      Machine(..ctx, display: handle_display(NotEnoughMoney(amount)))
    }
    True, True -> {
      let change = ctx.payment - item.cost
      Machine(
        ..ctx,
        items: ctx.items
          |> list.map(fn(x) {
            case x.name == item.name {
              False -> x
              True -> Item(..x, stock: x.stock - 1)
            }
          }),
        display: handle_display(ItemSelectSuccess(item.name, change)),
        payment: 0,
      )
    }
  }
}

pub fn user_event_handler(
  ctx: Machine,
  e: UserEvent,
  opt: String,
) -> Result(Machine, String) {
  case e {
    SelectItem -> {
      case list.find(ctx.items, fn(x) { x.name == opt }) {
        Error(_) -> Error(handle_display(NoItem))
        Ok(item) -> Ok(handle_item_selection(ctx, item))
      }
    }
    InputMoney -> {
      case int.parse(opt) {
        Error(_) -> Error(handle_display(InvalidMoney))
        Ok(money_amt) -> {
          let payment = ctx.payment + money_amt
          Ok(
            Machine(
              ..ctx,
              payment: payment,
              display: handle_display(MoneyInputSuccess(payment)),
            ),
          )
        }
      }
    }
    MoneyReturn ->
      Ok(
        Machine(
          ..ctx,
          payment: 0,
          display: handle_display(MoneyReturnSuccess(ctx.payment)),
        ),
      )
  }
}

pub fn update(ctx: Machine, e: String) -> Machine {
  case parse_user_event(e) {
    Error(m) -> Machine(..ctx, display: m)
    Ok(#(e, opt)) ->
      case user_event_handler(ctx, e, opt) {
        Error(m) -> Machine(..ctx, display: m)
        Ok(ctx) -> ctx
      }
  }
}
