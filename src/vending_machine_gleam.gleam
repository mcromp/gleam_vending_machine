// import gleam/io

type Money =
  Int

pub type MachineState {
  ItemSelectedAwaitingMoney
  MoneyInputedAwaitingItemSelection
  AwaitingItemAndMoney
}

pub type Machine {
  Machine(state: MachineState, bank: Money, payment: Money, display: String)
}

pub fn init() -> Machine {
  Machine(state: AwaitingItemAndMoney, bank: 0, payment: 0, display: "start")
}

pub fn update(ctx: Machine) -> Machine {
  Machine(..ctx, display: "changed")
}
