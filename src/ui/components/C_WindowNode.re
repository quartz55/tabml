open Relude.Globals;
open Browser.Globals;

module Styles = {
  open Css;

  let base = hovered =>
    style([
      display(flexBox),
      flexDirection(row),
      backgroundColor(hovered ? lightskyblue : lightblue),
    ]);

  let text = focused =>
    style([color(black), fontWeight(focused ? bold : normal)]);

  let title = style([color(rgba(0, 0, 0, 0.5))]);
};

[@react.component]
let make = (~win, ~hovered) => {
  let id = BWindow.id(win);
  let name = Format.sprintf("Window %d", id);
  let title = win.BWindow.title;
  let focused = win.BWindow.focused;
  <span className={Styles.base(hovered)}>
    <span className={Styles.text(focused)}> {React.string(name)} </span>
    {switch (title) {
     | None => React.null
     | Some(title) =>
       <span className=Styles.title> {React.string(title)} </span>
     }}
  </span>;
};