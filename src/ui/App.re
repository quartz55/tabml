open Relude.Globals;
open Browser.Globals;
open Core.Globals;
open UI_Components;

let str = React.string;

let debug = false;

let showPath: list(Relude.TreeZipper.movement) => string =
  List.reverse
  >> List.showBy(
       fun
       | `Down(x) => Format.sprintf("D%d", x)
       | `Right(x) => Format.sprintf("R%d", x)
       | _ => "_",
     );

module Tree = {
  [@react.component]
  let make = (~tree: Core.Tree.t) => <TreeNode node=tree path=[] />;
};

module Styles = {
  open Css;
  global("body", [backgroundColor(rgb(32, 32, 32))]);

  global("html", [fontSize(px(1))]);

  let app =
    style([
      fontFamilies([`custom("Helvetica Neue"), `custom("Helvetica")]),
      fontSize(rem(14.)),
    ]);
};

type coreConnection =
  | Init
  | Connected(BPort.t(Core.Tree.t))
  | Disconnected(BPort.t(Core.Tree.t));

type state = {
  core: coreConnection,
  tree: option(Core.Tree.t),
};

let port = Browser.Runtime.connect();

[@react.component]
let make = () => {
  let (state, setState) =
    React.useState(() => {core: Connected(port), tree: None});

  let fn = tree => setState(({core, _}) => {core, tree: Some(tree)});
  React.useEffect(() => {
    port.BPort.onMessage |> Browser.Types.EventListener.addListener(fn);
    Some(
      () =>
        port.BPort.onMessage |> Browser.Types.EventListener.removeListener(fn),
    );
  });

  <div className=Styles.app>
    {switch (state.tree) {
     | None => <div> {str("Initializing...")} </div>
     | Some(tree) => <div> <Tree tree /> </div>
     }}
  </div>;
};