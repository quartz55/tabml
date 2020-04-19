open Globals;
open Core.Globals;

type dispatch = CClient.Action.t => unit;

let getKey = node =>
  switch (Relude.Tree.getValue(node) |> CTNode.type_) {
  | Session(name) => "session-" ++ name
  | Window(w) => "window-" ++ Option.Infix.(string_of_int <$> w.id |? "none")
  | Tab(t) => "tab-" ++ Option.Infix.(string_of_int <$> t.id |? "none")
  };

module Styles = {
  open Css;

  let nodeWrapper = (~hovered) =>
    style([
      position(relative),
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

  let nodeContainer = style([display(flexBox)]);

  let toolboxContainer =
    style([
      position(absolute),
      right(zero),
      top(zero),
      height(`percent(100.)),
      display(flexBox),
      alignItems(stretch),
    ]);

  let childContainer =
    style([
      display(flexBox),
      flexDirection(column),
      paddingLeft(px(12)),
      borderLeft(px(1), dotted, lightgrey),
    ]);
};

type treeNodeProps = {
  .
  "node": Core.Tree.t,
  "path": option(CTree.path),
  "dispatch": dispatch,
};

module type TREE_NODE = {
  let makeProps:
    (
      ~node: Core.Tree.t,
      ~path: CTree.path=?,
      ~dispatch: dispatch,
      ~key: string=?,
      unit
    ) =>
    treeNodeProps;
  let make: treeNodeProps => React.element;
};

module rec TreeNode: TREE_NODE = {
  [@react.component]
  let make = (~node: CTree.t, ~path: CTree.path=[], ~dispatch: dispatch) => {
    let (curr, children) = Relude.Tree.(getValue(node), getChildren(node));
    let onClick = () =>
      Js.log2("Selecting", CTree.T.getValue(node) |> CTNode.Show.show);
    let onDoubleClick = () => {
      Logger.info(m =>
        m("Activating %s", CTree.T.getValue(node) |> CTNode.Show.show)
      );
      dispatch(Activate(path));
    };
    let (onClick, onDoubleClick) =
      Hooks.useClickHandler(~delay=250, onClick, onDoubleClick);
    let (hovered, setHovered) = React.useState(() => false);

    let expandedState =
      switch (List.isNotEmpty(children), CTNode.isCollapsed(curr)) {
      | (false, _) => `Disabled
      | (_, collapsed) => collapsed ? `Collapsed : `Expanded
      };

    let onDragStart = (ev: ReactEvent.Mouse.t) => {
      Logger.info(m =>
        m(
          "started dragging node %a at %s",
          CTNode.pp,
          curr,
          CTree.showPath(path),
        )
      );
    };

    <div>
      <div
        className={Styles.nodeWrapper(~hovered)}
        draggable=true
        onDragStart
        onMouseEnter={_ => setHovered(const(true))}
        onMouseLeave={_ => setHovered(const(false))}>
        <div className=Styles.nodeContainer>
          <ExpandButton
            state=expandedState
            onClick={() => dispatch(ToggleCollapse(path))}
          />
          /* */
          <HSpacer width={`px(5)} />
          /* */
          <div onClick onDoubleClick>
            {switch (CTNode.type_(curr)) {
             | Tab(tab) => <TabNode tab hovered />
             | Window(win) => <WindowNode win hovered />
             | Session(name) => <SessionNode name />
             }}
          </div>
        </div>
        /* */
        {hovered
           ? <div className=Styles.toolboxContainer>
               <NodeToolbox node=curr path dispatch />
             </div>
           : React.null}
      </div>
      {CTNode.isCollapsed(curr)
         ? React.null
         : <div className=Styles.childContainer>
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

                   <TreeNode key={getKey(child)} node=child path dispatch />;
                 })
              |> Relude.List.toArray
              |> React.array}
           </div>}
    </div>;
  };
};