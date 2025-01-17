(() => {
  // build/dev/javascript/prelude.mjs
  var CustomType = class {
    withFields(fields) {
      let properties = Object.keys(this).map(
        (label) => label in fields ? fields[label] : this[label]
      );
      return new this.constructor(...properties);
    }
  };
  var List = class {
    static fromArray(array, tail) {
      let t = tail || new Empty();
      for (let i = array.length - 1; i >= 0; --i) {
        t = new NonEmpty(array[i], t);
      }
      return t;
    }
    [Symbol.iterator]() {
      return new ListIterator(this);
    }
    toArray() {
      return [...this];
    }
    // @internal
    atLeastLength(desired) {
      for (let _ of this) {
        if (desired <= 0) return true;
        desired--;
      }
      return desired <= 0;
    }
    // @internal
    hasLength(desired) {
      for (let _ of this) {
        if (desired <= 0) return false;
        desired--;
      }
      return desired === 0;
    }
    // @internal
    countLength() {
      let length2 = 0;
      for (let _ of this) length2++;
      return length2;
    }
  };
  function prepend(element, tail) {
    return new NonEmpty(element, tail);
  }
  function toList(elements, tail) {
    return List.fromArray(elements, tail);
  }
  var ListIterator = class {
    #current;
    constructor(current) {
      this.#current = current;
    }
    next() {
      if (this.#current instanceof Empty) {
        return { done: true };
      } else {
        let { head, tail } = this.#current;
        this.#current = tail;
        return { value: head, done: false };
      }
    }
  };
  var Empty = class extends List {
  };
  var NonEmpty = class extends List {
    constructor(head, tail) {
      super();
      this.head = head;
      this.tail = tail;
    }
  };
  var Result = class _Result extends CustomType {
    // @internal
    static isResult(data) {
      return data instanceof _Result;
    }
  };
  var Ok = class extends Result {
    constructor(value) {
      super();
      this[0] = value;
    }
    // @internal
    isOk() {
      return true;
    }
  };
  var Error = class extends Result {
    constructor(detail) {
      super();
      this[0] = detail;
    }
    // @internal
    isOk() {
      return false;
    }
  };

  // build/dev/javascript/gleam_stdlib/gleam/list.mjs
  function reverse_loop(loop$remaining, loop$accumulator) {
    while (true) {
      let remaining = loop$remaining;
      let accumulator = loop$accumulator;
      if (remaining.hasLength(0)) {
        return accumulator;
      } else {
        let item = remaining.head;
        let rest$1 = remaining.tail;
        loop$remaining = rest$1;
        loop$accumulator = prepend(item, accumulator);
      }
    }
  }
  function reverse(list) {
    return reverse_loop(list, toList([]));
  }
  function map_loop(loop$list, loop$fun, loop$acc) {
    while (true) {
      let list = loop$list;
      let fun = loop$fun;
      let acc = loop$acc;
      if (list.hasLength(0)) {
        return reverse(acc);
      } else {
        let first$1 = list.head;
        let rest$1 = list.tail;
        loop$list = rest$1;
        loop$fun = fun;
        loop$acc = prepend(fun(first$1), acc);
      }
    }
  }
  function map(list, fun) {
    return map_loop(list, fun, toList([]));
  }
  function find(loop$list, loop$is_desired) {
    while (true) {
      let list = loop$list;
      let is_desired = loop$is_desired;
      if (list.hasLength(0)) {
        return new Error(void 0);
      } else {
        let x = list.head;
        let rest$1 = list.tail;
        let $ = is_desired(x);
        if ($) {
          return new Ok(x);
        } else {
          loop$list = rest$1;
          loop$is_desired = is_desired;
        }
      }
    }
  }

  // build/dev/javascript/gleam_stdlib/gleam/result.mjs
  function unwrap(result, default$) {
    if (result.isOk()) {
      let v = result[0];
      return v;
    } else {
      return default$;
    }
  }

  // build/dev/javascript/gleam_stdlib/dict.mjs
  var tempDataView = new DataView(new ArrayBuffer(8));
  var SHIFT = 5;
  var BUCKET_SIZE = Math.pow(2, SHIFT);
  var MASK = BUCKET_SIZE - 1;
  var MAX_INDEX_NODE = BUCKET_SIZE / 2;
  var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
  var unequalDictSymbol = Symbol();

  // build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
  var Nil = void 0;
  function parse_int(value) {
    if (/^[-+]?(\d+)$/.test(value)) {
      return new Ok(parseInt(value));
    } else {
      return new Error(Nil);
    }
  }
  function to_string(term) {
    return term.toString();
  }
  function split_once(haystack, needle) {
    const index = haystack.indexOf(needle);
    if (index >= 0) {
      const before = haystack.slice(0, index);
      const after = haystack.slice(index + needle.length);
      return new Ok([before, after]);
    } else {
      return new Error(Nil);
    }
  }
  var unicode_whitespaces = [
    " ",
    // Space
    "	",
    // Horizontal tab
    "\n",
    // Line feed
    "\v",
    // Vertical tab
    "\f",
    // Form feed
    "\r",
    // Carriage return
    "\x85",
    // Next line
    "\u2028",
    // Line separator
    "\u2029"
    // Paragraph separator
  ].join("");
  var trim_start_regex = new RegExp(`^[${unicode_whitespaces}]*`);
  var trim_end_regex = new RegExp(`[${unicode_whitespaces}]*$`);

  // build/dev/javascript/vending_machine_gleam/vending_machine_gleam.mjs
  var Item = class extends CustomType {
    constructor(cost, name, stock) {
      super();
      this.cost = cost;
      this.name = name;
      this.stock = stock;
    }
  };
  var SelectItem = class extends CustomType {
  };
  var InputMoney = class extends CustomType {
  };
  var MoneyReturn = class extends CustomType {
  };
  var Machine = class extends CustomType {
    constructor(bank, payment, display, items) {
      super();
      this.bank = bank;
      this.payment = payment;
      this.display = display;
      this.items = items;
    }
  };
  var ItemOutOfStock = class extends CustomType {
  };
  var MoneyInputSuccess = class extends CustomType {
    constructor(amount) {
      super();
      this.amount = amount;
    }
  };
  var ItemSelectSuccess = class extends CustomType {
    constructor(name, change) {
      super();
      this.name = name;
      this.change = change;
    }
  };
  var NotEnoughMoney = class extends CustomType {
    constructor(amount) {
      super();
      this.amount = amount;
    }
  };
  var NoItem = class extends CustomType {
  };
  var MoneyReturnSuccess = class extends CustomType {
    constructor(amount) {
      super();
      this.amount = amount;
    }
  };
  var InvalidMoney = class extends CustomType {
  };
  var Inital = class extends CustomType {
  };
  function handle_display(s) {
    if (s instanceof Inital) {
      return "Hello! Awaiting Input";
    } else if (s instanceof ItemOutOfStock) {
      return "Selected item out of stock";
    } else if (s instanceof ItemSelectSuccess) {
      let name = s.name;
      let change = s.change;
      return "Here is a " + name + ". Your change is " + (() => {
        let _pipe = change;
        return to_string(_pipe);
      })();
    } else if (s instanceof MoneyInputSuccess) {
      let amount = s.amount;
      return "Total : " + (() => {
        let _pipe = amount;
        return to_string(_pipe);
      })();
    } else if (s instanceof NoItem) {
      return "Item not found";
    } else if (s instanceof NotEnoughMoney) {
      let amount = s.amount;
      return "You require " + (() => {
        let _pipe = amount;
        return to_string(_pipe);
      })() + " more";
    } else if (s instanceof MoneyReturnSuccess) {
      let amount = s.amount;
      return (() => {
        let _pipe = amount;
        return to_string(_pipe);
      })() + " returned";
    } else {
      return "Invalid Money Input";
    }
  }
  function init() {
    return new Machine(
      0,
      0,
      handle_display(new Inital()),
      toList([
        new Item(150, "tea", 10),
        new Item(100, "orange soda", 3),
        new Item(200, "purple soda", 20),
        new Item(250, "water", 10)
      ])
    );
  }
  function parse_user_event(e) {
    let $ = (() => {
      let _pipe = e;
      let _pipe$1 = split_once(_pipe, "/");
      return unwrap(_pipe$1, ["", ""]);
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
  function handle_item_selection(ctx, item) {
    let $ = item.stock > 0;
    let $1 = ctx.payment >= item.cost;
    if (!$) {
      let _record = ctx;
      return new Machine(
        _record.bank,
        _record.payment,
        handle_display(new ItemOutOfStock()),
        _record.items
      );
    } else if (!$1) {
      let amount = item.cost - ctx.payment;
      let _record = ctx;
      return new Machine(
        _record.bank,
        _record.payment,
        handle_display(new NotEnoughMoney(amount)),
        _record.items
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
          return map(
            _pipe,
            (x) => {
              let $2 = x.name === item.name;
              if (!$2) {
                return x;
              } else {
                let _record$1 = x;
                return new Item(_record$1.cost, _record$1.name, x.stock - 1);
              }
            }
          );
        })()
      );
    }
  }
  function user_event_handler(ctx, e, opt) {
    if (e instanceof SelectItem) {
      let $ = find(ctx.items, (x) => {
        return x.name === opt;
      });
      if (!$.isOk()) {
        return new Error(handle_display(new NoItem()));
      } else {
        let item = $[0];
        return new Ok(handle_item_selection(ctx, item));
      }
    } else if (e instanceof InputMoney) {
      let $ = parse_int(opt);
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
              _record.items
            );
          })()
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
            _record.items
          );
        })()
      );
    }
  }
  function update(ctx, e) {
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
})();
