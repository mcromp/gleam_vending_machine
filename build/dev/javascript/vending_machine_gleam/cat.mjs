import * as $json from "../gleam_json/gleam/json.mjs";
import * as $decode from "../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import { CustomType as $CustomType } from "./gleam.mjs";

export class S extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export function cat_from_json(_) {
  let g = new S("hello");
  $io.debug("##");
  $io.debug(g);
  return g;
}
