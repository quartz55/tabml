open Relude.Globals;
open Browser.Globals;

module Styles = {
  open Css;

  let base =
    style([
      display(flexBox),
      flexDirection(row),
      backgroundColor(darkgrey),
    ]);

  let text = style([color(lightgrey)]);
};

[@react.component]
let make = (~name) => {
  <span className=Styles.base>
    <span className=Styles.text> {React.string(name)} </span>
  </span>;
};