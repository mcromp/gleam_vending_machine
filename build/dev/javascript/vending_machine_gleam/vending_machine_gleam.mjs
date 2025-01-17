import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, CustomType as $CustomType } from "./gleam.mjs";

export class Item extends $CustomType {
  constructor(cost, name, stock) {
    super();
    this.cost = cost;
    this.name = name;
    this.stock = stock;
  }
}

export class SelectItem extends $CustomType {}

export class InputMoney extends $CustomType {}

export class MoneyReturn extends $CustomType {}

export class Machine extends $CustomType {
  constructor(bank, payment, display, items) {
    super();
    this.bank = bank;
    this.payment = payment;
    this.display = display;
    this.items = items;
  }
}

class ItemOutOfStock extends $CustomType {}

class MoneyInputSuccess extends $CustomType {
  constructor(amount) {
    super();
    this.amount = amount;
  }
}

class ItemSelectSuccess extends $CustomType {
  constructor(name, change) {
    super();
    this.name = name;
    this.change = change;
  }
}

class NotEnoughMoney extends $CustomType {
  constructor(amount) {
    super();
    this.amount = amount;
  }
}

class NoItem extends $CustomType {}

class MoneyReturnSuccess extends $CustomType {
  constructor(amount) {
    super();
    this.amount = amount;
  }
}

class InvalidMoney extends $CustomType {}

class Inital extends $CustomType {}

function handle_display(s) {
  if (s instanceof Inital) {
    return "Hello! Awaiting Input";
  } else if (s instanceof ItemOutOfStock) {
    return "Selected item out of stock";
  } else if (s instanceof ItemSelectSuccess) {
    let name = s.name;
    let change = s.change;
    return (("Here is a " + name) + ". Your change is ") + (() => {
      let _pipe = change;
      return $int.to_string(_pipe);
    })();
  } else if (s instanceof MoneyInputSuccess) {
    let amount = s.amount;
    return "Total : " + (() => {
      let _pipe = amount;
      return $int.to_string(_pipe);
    })();
  } else if (s instanceof NoItem) {
    return "Item not found";
  } else if (s instanceof NotEnoughMoney) {
    let amount = s.amount;
    return ("You require " + (() => {
      let _pipe = amount;
      return $int.to_string(_pipe);
    })()) + " more";
  } else if (s instanceof MoneyReturnSuccess) {
    let amount = s.amount;
    return (() => {
      let _pipe = amount;
      return $int.to_string(_pipe);
    })() + " returned";
  } else {
    return "Invalid Money Input";
  }
}

export function init() {
  return new Machine(
    0,
    0,
    handle_display(new Inital()),
    toList([
      new Item(150, "tea", 10),
      new Item(100, "orange soda", 3),
      new Item(200, "purple soda", 20),
      new Item(250, "water", 10),
    ]),
  );
}

export function parse_user_event(e) {
  let $ = (() => {
    let _pipe = e;
    let _pipe$1 = $string.split_once(_pipe, "/");
    return $result.unwrap(_pipe$1, ["", ""]);
  })();
  let code = $[0];
  let opt = $[1];
  if (code === "0") {
    return new Ok([new SelectItem(), opt]);
  } else if (code === "1") {
    return new Ok([new InputMoney(), opt]);
  } else if (code === "2") {
    return new Ok([new MoneyReturn(), opt]);
  } else {
    return new Error("Unrecognized User Event");
  }
}

export function handle_item_selection(ctx, item) {
  let $ = item.stock > 0;
  let $1 = ctx.payment >= item.cost;
  if (!$) {
    let _record = ctx;
    return new Machine(
      _record.bank,
      _record.payment,
      handle_display(new ItemOutOfStock()),
      _record.items,
    );
  } else if (!$1) {
    let amount = item.cost - ctx.payment;
    let _record = ctx;
    return new Machine(
      _record.bank,
      _record.payment,
      handle_display(new NotEnoughMoney(amount)),
      _record.items,
    );
  } else {
    let change = ctx.payment - item.cost;
    let _record = ctx;
    return new Machine(
      _record.bank,
      0,
      handle_display(new ItemSelectSuccess(item.name, change)),
      (() => {
        let _pipe = ctx.items;
        return $list.map(
          _pipe,
          (x) => {
            let $2 = x.name === item.name;
            if (!$2) {
              return x;
            } else {
              let _record$1 = x;
              return new Item(_record$1.cost, _record$1.name, x.stock - 1);
            }
          },
        );
      })(),
    );
  }
}

export function user_event_handler(ctx, e, opt) {
  if (e instanceof SelectItem) {
    let $ = $list.find(ctx.items, (x) => { return x.name === opt; });
    if (!$.isOk()) {
      return new Error(handle_display(new NoItem()));
    } else {
      let item = $[0];
      return new Ok(handle_item_selection(ctx, item));
    }
  } else if (e instanceof InputMoney) {
    let $ = $int.parse(opt);
    if (!$.isOk()) {
      return new Error(handle_display(new InvalidMoney()));
    } else {
      let money_amt = $[0];
      let payment = ctx.payment + money_amt;
      return new Ok(
        (() => {
          let _record = ctx;
          return new Machine(
            _record.bank,
            payment,
            handle_display(new MoneyInputSuccess(payment)),
            _record.items,
          );
        })(),
      );
    }
  } else {
    return new Ok(
      (() => {
        let _record = ctx;
        return new Machine(
          _record.bank,
          0,
          handle_display(new MoneyReturnSuccess(ctx.payment)),
          _record.items,
        );
      })(),
    );
  }
}

export function update(ctx, e) {
  let $ = parse_user_event(e);
  if (!$.isOk()) {
    let m = $[0];
    let _record = ctx;
    return new Machine(_record.bank, _record.payment, m, _record.items);
  } else {
    let e$1 = $[0][0];
    let opt = $[0][1];
    let $1 = user_event_handler(ctx, e$1, opt);
    if (!$1.isOk()) {
      let m = $1[0];
      let _record = ctx;
      return new Machine(_record.bank, _record.payment, m, _record.items);
    } else {
      let ctx$1 = $1[0];
      return ctx$1;
    }
  }
}
