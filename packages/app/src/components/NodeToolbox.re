open Globals;
open Core.Globals;

module Button = {
  module Styles = {
    open Css;
    let wrapper = (fg, bg) =>
      style([
        display(inlineFlex),
        alignItems(center),
        padding(px(2)),
        paddingLeft(px(4)),
        paddingRight(px(4)),
        color(fg),
        backgroundColor(bg),
        cursor(pointer),
        fontWeight(bold),
      ]);
  };

  [@react.component]
  let make =
      (~fg=Css.white, ~bg=Css.rgb(32, 32, 32), ~icon, ~tooltip, ~onClick) =>
    <div className={Styles.wrapper(fg, bg)} onClick={_ => onClick()}>
      {React.string(icon)}
    </div>;
};

module Styles = {
  open Css;

  let wrapper =
    style([
      display(flexBox),
      flexDirection(row),
      backgroundColor(rgb(62, 62, 62)),
      padding4(~top=px(2), ~bottom=px(2), ~left=px(8), ~right=px(4)),
    ]);
};

[@react.component]
let make =
    (~node: CTNode.t, ~path: CTree.path, ~dispatch: CClient.Action.t => unit) => {
  <div
    className=Styles.wrapper
    onClick=ReactEvent.Mouse.stopPropagation
    onDoubleClick=ReactEvent.Mouse.stopPropagation>
    <Button
      bg={Css.rgb(200, 32, 32)}
      icon="X"
      tooltip="Remove node"
      onClick={() => dispatch(Remove(path))}
    />
    <HSpacer width={Css.px(4)} />
    <Button
      bg=Css.transparent
      fg={Css.rgb(32, 200, 32)}
      icon="X"
      tooltip="Suspend node"
      onClick={() => ()}
    />
  </div>;
};