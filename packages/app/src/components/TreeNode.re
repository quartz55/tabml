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

  let treeNodeContainer = style([width(`percent(100.))]);

  let nodeWrapper = (~hovered, ~dropping) =>
    style([
      position(relative),
      backgroundColor(
        hovered
          ? rgba(255, 255, 255, `num(0.3))
          : dropping ? rgba(32, 255, 32, `num(0.3)) : transparent,
      ),
      padding(px(4)),
      paddingLeft(px(6)),
      paddingRight(px(6)),
      borderRadius(px(4)),
      cursor(`default),
      userSelect(`none),
      textAlign(`left),
      width(`percent(100.)),
    ]);

  let dropZone = (~active) =>
    style([
      pointerEvents(active ? auto : none),
      zIndex(9999),
      position(absolute),
      left(zero),
      top(zero),
      width(`percent(100.)),
      height(`percent(100.)),
      // backgroundColor(rgba(32, 32, 255, `num(0.1))),
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

  let childrenContainer =
    style([
      display(flexBox),
      flexDirection(column),
      borderLeft(px(1), dotted, lightgrey),
    ]);

  let childContainer = style([display(flexBox), flexDirection(row)]);

  let siblingDropzone = (~dropping) =>
    style([
      position(relative),
      width(`px(15)),
      backgroundColor(
        dropping ? rgba(32, 255, 32, `num(0.3)) : transparent,
      ),
    ]);
};

module DropZone = {
  [@react.component]
  let make = (~active, ~onActivity) => {
    let makeHandler = (h, ev) => {
      h(ev);
      ReactEvent.Mouse.preventDefault(ev);
    };

    <div
      className={Styles.dropZone(~active)}
      onDragEnter={makeHandler(_ => onActivity(true))}
      onDragLeave={makeHandler(_ => onActivity(false))}
      onDragOver={makeHandler(const())}
      onClick=ReactEvent.Mouse.preventDefault
      onDoubleClick=ReactEvent.Mouse.preventDefault
    />;
  };
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
    let dragging = Store.Dnd.useStoreWithSelector(s => s != Inactive, ());
    let hovering =
      Store.Dnd.useStoreWithSelector(
        s =>
          switch (s) {
          | Hovering(_, p, s) => Some((p, s))
          | _ => None
          },
        ~areEqual=
          (a, b) =>
            switch (a, b) {
            | (Some((a, true)), Some((b, true)))
            | (Some((a, false)), Some((b, false))) => CTree.Path.eq(a, b)
            | (None, None) => true
            | _ => false
            },
        (),
      );

    let dropping =
      switch (hovering) {
      | Some((p, false)) => CTree.Path.eq(p, path)
      | _ => false
      };

    let expandedState =
      switch (List.isNotEmpty(children), CTNode.isCollapsed(curr)) {
      | (false, _) => `Disabled
      | (_, collapsed) => collapsed ? `Collapsed : `Expanded
      };

    let onDragStart = (_ev: ReactEvent.Mouse.t) => {
      setHovered(const(false));
      Store.Dnd.dispatch(BeginDrag(path));
      Logger.info(m =>
        m(
          "started dragging node %a at %s",
          CTNode.pp,
          curr,
          CTree.showPath(path),
        )
      );
      // let f: (string, ReactEvent.Mouse.t) => unit = [%raw
      //   {|(v, event) => event.dataTransfer.setData('text/plain', v)|}
      // ];
      // f(CTree.Path.show(path), ev);
    };
    let onDragEnd = (_ev: ReactEvent.Mouse.t) => {
      Store.Dnd.dispatch(Drop);
    };

    let handleNodeDropActivity = activity => {
      Logger.debug(m =>
        m(
          "%s drop zone of node %a at %s",
          activity ? "entered" : "left",
          CTNode.pp,
          curr,
          CTree.showPath(path),
        )
      );
      activity
        ? Store.Dnd.dispatch(Hovering(path))
        : Store.Dnd.dispatch(Leave(path));
    };

    <div className=Styles.treeNodeContainer>
      <div
        className={Styles.nodeWrapper(~hovered, ~dropping)}
        draggable=true
        onDragStart
        onDragEnd
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
        <DropZone active=dragging onActivity=handleNodeDropActivity />
        /* */
        {hovered
           ? <div className=Styles.toolboxContainer>
               <NodeToolbox node=curr path dispatch />
             </div>
           : React.null}
      </div>
      {CTNode.isCollapsed(curr)
         ? React.null
         : <div className=Styles.childrenContainer>
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

                   let handleSiblingDropActivity = activity =>
                     Store.Dnd.dispatch(
                       activity ? HoveringSibling(path) : LeaveSibling(path),
                     );

                   let dropping =
                     switch (hovering) {
                     | Some((p, true)) => CTree.Path.eq(p, path)
                     | _ => false
                     };

                   <div key={getKey(child)} className=Styles.childContainer>
                     <div className={Styles.siblingDropzone(~dropping)}>
                       <DropZone
                         active=dragging
                         onActivity=handleSiblingDropActivity
                       />
                     </div>
                     <TreeNode node=child path dispatch />
                   </div>;
                 })
              |> Relude.List.toArray
              |> React.array}
           </div>}
    </div>;
  };
};
