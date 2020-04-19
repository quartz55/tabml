open Globals;
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

type state = {
  core: BPort.t(CClient.Action.t, CTree.t),
  tree: option(Core.Tree.t),
};

let port =
  B.Runtime.(connect(~connectInfo=makeConInfo(~name="main", ()), ()));

[@react.component]
let make = () => {
  let (state, setState) = React.useState(() => {core: port, tree: None});
  let fn =
    React.useCallback0(tree =>
      setState(({core, _}) => {core, tree: Some(tree)})
    );
  React.useEffect0(() => {
    port.BPort.onMessage |> EL.addListener(fn);
    Some(() => port.BPort.onMessage |> EL.removeListener(fn));
  });
  let dispatch =
    React.useCallback1(
      action => {port |> BPort.postMessage(action)},
      [|port|],
    );

  <div className=Styles.app>
    {switch (state.tree) {
     | None => <div> {str("Initializing...")} </div>
     | Some(tree) => <div> <Tree tree dispatch /> </div>
     }}
  </div>;
};