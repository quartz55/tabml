open Globals;

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

  let title =
    style([
      textOverflow(`ellipsis),
      overflow(hidden),
      whiteSpace(nowrap),
      flexGrow(0.),
      color(rgba(0, 0, 0, 0.5)),
    ]);
};

[@react.component]
let make = (~win: BWindow.t, ~hovered: bool) => {
  let id = Option.getOrThrow(win.id);
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