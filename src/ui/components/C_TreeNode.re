open Relude.Globals;
open Core.Globals;
open Browser.Globals;

module Styles = {
  open Css;

  let nodeWrapper = (~hovered) =>
    style([
      display(flexBox),
      flexDirection(row),
      backgroundColor(hovered ? rgba(255, 255, 255, 0.3) : transparent),
      padding(px(4)),
      paddingLeft(px(6)),
      paddingRight(px(6)),
      borderRadius(px(4)),
      cursor(`default),
      userSelect(`none),
      textAlign(`left),
      width(`percent(100.)),
    ]);

  let childContainer =
    style([
      display(flexBox),
      flexDirection(column),
      paddingLeft(px(12)),
      borderLeft(px(1), dotted, lightgrey),
    ]);
};

type nodePath = list(Relude.TreeZipper.movement);

type treeNodeProps = {
  .
  "node": Core.Tree.t,
  "path": option(nodePath),
};

module type TREE_NODE = {
  let makeProps:
    (~node: Core.Tree.t, ~path: nodePath=?, ~key: string=?, unit) =>
    treeNodeProps;
  let make: treeNodeProps => React.element;
};

module rec TreeNode: TREE_NODE = {
  let getKey = node =>
    switch (Relude.Tree.getValue(node) |> CTNode.type_) {
    | Session(name) => "session-" ++ name
    | Window(w) =>
      "window-" ++ Option.Infix.(string_of_int <$> BWindow.idOpt(w) |? "none")
    | Tab(t) =>
      "tab-" ++ Option.Infix.(string_of_int <$> BTab.idOpt(t) |? "none")
    };

  [@react.component]
  let make = (~node: CTree.t, ~path: nodePath=[]) => {
    let (curr, children) = Relude.Tree.(getValue(node), getChildren(node));
    let onClick = () =>
      Js.log2("Selecting", CTree.T.getValue(node) |> CTNode.Show.show);
    let onDoubleClick = () =>
      Js.log2("Activating", CTree.T.getValue(node) |> CTNode.Show.show);
    let (onClick, onDoubleClick) =
      UI_Hooks.useClickHandler(~delay=250, onClick, onDoubleClick);
    let (hovered, setHovered) = React.useState(() => false);

    <div>
      <button
        className={Styles.nodeWrapper(~hovered)}
        onClick
        onDoubleClick
        onMouseEnter={_ => setHovered(const(true))}
        onMouseLeave={_ => setHovered(const(false))}>
        {switch (CTNode.type_(curr)) {
         | Tab(tab) => <C_TabNode tab hovered />
         | Window(win) => <C_WindowNode win hovered />
         | Session(name) => <C_SessionNode name />
         }}
      </button>
      <div className=Styles.childContainer>
        {children
         |> Relude.List.mapWithIndex((child, idx) => {
              let path =
                path
                |> (
                  fun
                  | [`Down(x), ...p] => [`Down(x + 1), ...p]
                  | p => [`Down(1), ...p]
                )
                |> (p => Int.eq(idx, 0) ? p : [`Right(idx), ...p]);

              <TreeNode key={getKey(child)} node=child path />;
            })
         |> Relude.List.toArray
         |> React.array}
      </div>
    </div>;
  };
};