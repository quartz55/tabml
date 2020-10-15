open Core.Globals;

let str = React.string;

let debug = false;

module Tree = {
  [@react.component]
  let make = (~tree: Core.Tree.t, ~dispatch: CClient.Action.t => unit) =>
    <TreeNode.TreeNode node=tree path=[] dispatch />;
};

module Styles = {
  [%raw {|require("../assets/_reset.css")|}];
  open Css;
  global("body", [backgroundColor(rgb(32, 32, 32))]);

  let app =
    style([
      fontFamilies([`custom("Helvetica Neue"), `custom("Helvetica")]),
      fontSize(rem(0.875)),
    ]);
};

[@react.component]
let make = () => {
  let (tree, dispatch) = Store.(useCore(), dispatch);

  <div className=Styles.app>
    {switch (tree) {
     | None => <div> {str("Initializing...")} </div>
     | Some(tree) => <div> <Tree tree dispatch /> </div>
     }}
  </div>;
};
