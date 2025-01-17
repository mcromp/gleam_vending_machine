import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $gleeunit from "../gleeunit/gleeunit.mjs";
import * as $should from "../gleeunit/gleeunit/should.mjs";
import * as $cat from "./cat.mjs";

export function main() {
  return $gleeunit.main();
}

export function cat_test() {
  let qq = "hello";
  let cc = $cat.cat_from_json("EE");
  let cc$1 = "h";
  let _pipe = qq;
  return $should.equal(_pipe, cc$1);
}
