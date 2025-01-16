import gleam/int
import gleam/list
import gleam/string

type Money =
  Int

pub type Item {
  Item(cost: Money, name: String, stock: Int)
}

pub type Machine {
  Machine(bank: Money, payment: Money, display: String, items: List(Item))
}

pub fn init() -> Machine {
  Machine(bank: 0, payment: 0, display: "Awaiting input...", items: [
    Item(cost: 150, name: "Orange Soda", stock: 10),
  ])
}

pub type UserEvent {
  SelectItem
  InputMoney
  MoneyReturn
}

pub fn parse_user_event(e: String) -> Result(#(UserEvent, String), String) {
  case string.split_once(e, on: "/") {
    Ok(#(code, opt)) ->
      case code {
        "0" -> Ok(#(SelectItem, opt))
        "1" -> Ok(#(InputMoney, opt))
        "2" -> Ok(#(MoneyReturn, opt))
        _ -> Error("Unrecognized User Event")
      }
    Error(_) -> Error("User Event Failed to Parse")
  }
}

pub fn handle_item_selection(ctx: Machine, item: Item) {
  case item.stock > 0, ctx.payment > item.cost {
    False, _ -> Machine(..ctx, display: "Item out of stock")
    _, False ->
      Machine(
        ..ctx,
        display: "You require "
          <> item.cost - ctx.payment |> int.to_string
          <> " more",
      )
    True, True -> {
      let change = ctx.payment - item.cost |> int.to_string
      Machine(
        ..ctx,
        items: ctx.items
          |> list.map(fn(x) {
            case x.name == item.name {
              False -> x
              True -> Item(..x, stock: x.stock - 1)
            }
          }),
        display: "Here is a " <> item.name <> ". Your change is " <> change,
        payment: 0,
      )
    }
  }
}

pub fn user_event_handler(ctx: Machine, e: String) -> Machine {
  case parse_user_event(e) {
    Error(m) -> Machine(..ctx, display: m)
    Ok(#(e, opt)) -> {
      case e {
        SelectItem -> {
          case ctx.items |> list.find(fn(x) { x.name == opt }) {
            Error(_) -> Machine(..ctx, display: "Item not found")
            Ok(item) -> handle_item_selection(ctx, item)
          }
        }
        InputMoney -> {
          case int.parse(opt) {
            Error(_) -> Machine(..ctx, display: "Invalid Money Input")
            Ok(money_amt) -> {
              Machine(
                ..ctx,
                payment: ctx.payment + money_amt,
                display: "Balance: " <> ctx.payment + money_amt |> int.to_string,
              )
            }
          }
        }
        MoneyReturn -> {
          Machine(
            ..ctx,
            payment: 0,
            display: ctx.payment |> int.to_string <> " returned",
          )
        }
      }
    }
  }
}

pub fn update(ctx: Machine) -> Machine {
  Machine(..ctx, display: "changed")
}
