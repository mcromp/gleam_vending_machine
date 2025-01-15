import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/json
import gleam/result

// import gleam/json
// import gleam/option.{type Option, None, Some}

// import gleam/result
import gleam/string
import simplifile

// import gleam/io

type Money =
  Int

pub type MachineState {
  ItemSelectedAwaitingMoney
  MoneyInputedAwaitingItemSelection
  AwaitingItemAndMoney
}

pub type ItemType {
  OrangeSoda
  CherrySoda
  IceTea
}

pub type Item {
  Item(cost: Money, name: String)
}

pub type Machine {
  Machine(
    state: MachineState,
    bank: Money,
    payment: Money,
    display: String,
    items: List(Item),
  )
}

pub fn init() -> Machine {
  Machine(
    state: AwaitingItemAndMoney,
    bank: 0,
    payment: 0,
    display: "start",
    items: [Item(cost: 150, name: "Orange Soda")],
  )
}

// type Events {
//   Coin(money: Money)
//   Card
//   SelectItem(Item)
// }
type Actions {
  Actions(select_item: String, input_money: String, scan_card: String)
}

fn fetch_actions() -> Actions {
  let filepath = "./ACTIONS.json"

  let action_decoder = {
    use select_item <- decode.field("SELECT_ITEM", decode.string)
    use input_money <- decode.field("INPUT_MONEY", decode.string)
    use scan_card <- decode.field("INPUT_MONEY", decode.string)
    decode.success(Actions(select_item:, input_money:, scan_card:))
  }

  let assert Ok(json) = simplifile.read(filepath)
  let assert Ok(result) = json.parse(json, action_decoder)
  io.debug("##")
  io.debug(result)
  io.debug("##")
  result
}

pub fn user_event_handler(ctx: Machine, e: String) -> Machine {
  let q = fetch_actions()
  let assert Ok(#(code, opt)) = string.split_once(e, on: "/")
  io.debug("$$$$$,  " <> q.input_money)

  case code {
    select_item -> {
      case int.parse(opt) {
        Error(_) -> Machine(..ctx, display: "Invalid Item Selection")
        Ok(money_amt) -> {
          Machine(..ctx, payment: ctx.payment + money_amt)
        }
      }
    }
    input_money -> {
      case int.parse(opt) {
        Error(_) -> Machine(..ctx, display: "Invalid Money Input")
        Ok(money_amt) -> {
          Machine(..ctx, payment: ctx.payment + money_amt)
        }
      }
    }
    _ -> panic as "Scores should never be negative!"
  }
}

pub fn update(ctx: Machine) -> Machine {
  Machine(..ctx, display: "changed")
}
