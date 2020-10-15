open Globals;
open Core.Globals;

type action =
  | Dispatch(CClient.Action.t)
  | UpdateTree(CTree.t);

let (dispatch, useCore) = {
  let port: BPort.t(CClient.Action.t, CTree.t) =
    B.Runtime.(connect(~connectInfo=makeConInfo(~name="main", ()), ()));

  let Restorative.{dispatch, useStore} =
    Restorative.createStore(None, (s, action) =>
      switch (action) {
      | Dispatch(action) =>
        port |> BPort.postMessage(action);
        s;
      | UpdateTree(t) => Some(t)
      }
    );

  port.BPort.onMessage |> EL.addListener(tree => dispatch(UpdateTree(tree)));

  let dispatch = action => dispatch(Dispatch(action));

  (dispatch, () => useStore());
};

module Dnd = {
  type state =
    | Inactive
    | Dragging(CTree.path)
    | Hovering(CTree.path, CTree.path, bool);

  type action =
    | BeginDrag(CTree.path)
    | Hovering(CTree.path)
    | HoveringSibling(CTree.path)
    | Leave(CTree.path)
    | LeaveSibling(CTree.path)
    | Drop;

  let Restorative.{dispatch, useStore, useStoreWithSelector} =
    Restorative.createStore(Inactive, (state, action) =>
      switch (state, action) {
      | (Inactive, BeginDrag(path)) => Dragging(path)
      | (Dragging(p1) | Hovering(p1, _, _), Hovering(p2)) =>
        Hovering(p1, p2, false)
      | (Dragging(p1) | Hovering(p1, _, _), HoveringSibling(p2)) =>
        Hovering(p1, p2, true)
      | (Hovering(p1, p2, false), Leave(p3))
      | (Hovering(p1, p2, true), LeaveSibling(p3))
          when CTree.Path.eq(p2, p3) =>
        Dragging(p1)
      | (Hovering(p1, p2, sib), Drop) =>
        Js.log3(
          "dispatching move",
          CTree.Path.show(p1),
          CTree.Path.show(p2),
        );
        dispatch(Move(p1, p2, sib));
        Inactive;
      | (_, Drop) => Inactive
      | _ => state
      }
    );
};
