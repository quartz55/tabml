open Globals;

module Styles = {
  open Css;

  let favicon = style([width(em(1.)), height(em(1.))]);

  let base = style([display(flexBox), flexDirection(row)]);

  let text = (~active, ~hovered) =>
    style([
      color(white),
      fontWeight(active ? bold : normal),
      textShadow(hovered ? Shadow.text(~x=px(1), ~y=px(1), black) : `none),
    ]);
};

[@react.component]
let make = (~tab: BTab.t, ~hovered: bool) => {
  let t = tab;
  let text =
    Option.Infix.((id => "[" ++ Int.toString(id) ++ "] ") <$> t.id |? "")
    ++ Option.Infix.(t.title |? "untitled");
  let favicon =
    t.favIconUrl |> Option.getOrElse("chrome://branding/content/icon32.png");
  <span className=Styles.base>
    <img className=Styles.favicon src=favicon />
    <span className={Styles.text(~active=t.active, ~hovered)}>
      {React.string(text)}
    </span>
  </span>;
};