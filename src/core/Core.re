open Relude.Globals;
open Browser.Globals;
module Tree = Core_Tree;

type action =
  | CreateWindow(BWindow.t)
  | ActivateWindow(BT.windowId)
  | RemoveWindow(BT.windowId)
  | CreateTab(BTab.t)
  | ReplaceTab(BT.tabId, BT.tabId)
  | AttachTab(BT.tabId, int, int)
  | MoveTab(BT.tabId, BT.windowId, (int, int))
  | RemoveTab(BT.tabId)
  | ActivateTab(Browser.Tabs.activeInfo)
  | UpdateTab(BT.tabId, Browser.Tabs.changeInfo, BTab.t)
  | Update(Tree.t);

let reducer = (state, action) =>
  switch (action) {
  | CreateWindow(win) => state |> Tree.appendWindow(Tree.Node.ofWindow(win))
  | ActivateWindow(winId) =>
    state
    |> Tree.mapNode(n =>
         switch (n) {
         | Window({focused: true, _} as w) =>
           Some(Window({...w, focused: false}))
         | _ => None
         }
       )
    |> Tree.modifyWindowWithId(w => {...w, focused: true}, winId)
  | RemoveWindow(winId) => state |> Tree.removeWindowWithId(winId)
  | CreateTab(tab) => state |> Tree.addCreatedTab(tab) |> Option.getOrThrow
  | ReplaceTab(added, removed) =>
    Js.log2("UNHANDLED REPLACE TAB", (added, removed));
    state;
  | AttachTab(tabId, winId, idx) =>
    Format.printf(
      "attaching tab %d to window %d at idx %d@.",
      tabId,
      winId,
      idx,
    );
    Tree.moveTabWithIdToIdx(~window=`OfId(winId), tabId, idx, state)
    |> Option.getOrThrow;
  | MoveTab(tabId, winId, (fromIdx, toIdx)) =>
    Format.printf(
      "moving tab %d on window %d from idx %d to %d@.",
      tabId,
      winId,
      fromIdx,
      toIdx,
    );
    Tree.moveTabWithIdToIdx(tabId, toIdx, state) |> Option.getOrThrow;
  | RemoveTab(tabId) =>
    Js.log2("removing tab", tabId);
    state |> Tree.removeTabWithId(tabId);
  | ActivateTab({previousTabId, tabId, _}) =>
    Js.log2("activate tab", tabId);
    previousTabId
    |> Option.map(tabId => {
         Printf.printf("deactivating %d first", tabId);
         state |> Tree.modifyTabWithId(t => {...t, active: false}, tabId);
       })
    |> Option.getOrElse(state)
    |> Tree.modifyTabWithId(t => {...t, active: true}, tabId);
  | UpdateTab(tabId, changes, newTab) =>
    Js.log4("updating tab", tabId, newTab, changes);
    state |> Tree.setTabWithId(tabId, newTab);
  | Update(tree) =>
    Js.log("full tree update");
    tree;
  };

let main = () => {
  Tree.bootstrap()
  |> IO.map(tree => {
       Format.printf("%a", Tree.pp, tree);
       let state = ref(tree);
       let subs = ref([]);
       let handle = action => {
         state := reducer(state^, action);
         subs^ |> List.forEach(s => BPort.postMessage(s, state^));
         state^;
       };
       let dispatch = handle >> ignore;
       let _rebuild = _ =>
         Tree.bootstrap()
         |> IO.unsafeRunAsync(
              fun
              | Ok(t) => dispatch(Update(t))
              | Error(e) => Js.Console.error(e),
            );

       // Window handling
       Browser.Windows.onCreated
       |> EL.addListener(win => dispatch(CreateWindow(win)));
       Browser.Windows.onFocusChanged
       |> EL.addListener(winId => dispatch(ActivateWindow(winId)));
       Browser.Windows.onRemoved
       |> EL.addListener(winId => dispatch(RemoveWindow(winId)));

       // Tab handling
       Browser.Tabs.onCreated
       |> EL.addListener(tab => dispatch(CreateTab(tab)));
       Browser.Tabs.onReplaced
       |> EL.addListener((~added, removed) =>
            dispatch(ReplaceTab(added, removed))
          );
       Browser.Tabs.onAttached
       |> EL.addListener((tabId, ai) =>
            dispatch(AttachTab(tabId, ai.BTabs.newWindowId, ai.newPosition))
          );
       Browser.Tabs.onMoved
       |> EL.addListener((tabId, BTabs.{windowId, fromIndex, toIndex}) =>
            dispatch(MoveTab(tabId, windowId, (fromIndex, toIndex)))
          );
       Browser.Tabs.onRemoved
       |> EL.addListener(tabId => dispatch(RemoveTab(tabId)));
       Browser.Tabs.onActivated
       |> EL.addListener(info => dispatch(ActivateTab(info)));
       Browser.Tabs.onUpdated
       |> EL.addListener((tabId, changes, tab) =>
            dispatch(UpdateTab(tabId, changes, tab))
          );

       Browser.Runtime.onConnect
       |> EL.addListener(port => {
            BPort.postMessage(port, state^);
            port.onDisconnect
            |> EL.addListener(_p => {
                 subs := subs^ |> List.reject([%raw "x => x == port"])
               });
            subs := [port, ...subs^];
          });
     })
  |> IO.unsafeRunAsync(Js.log);
};

module Globals = {
  module CTree = Core_Tree;
  module CTZip = Core_Tree.TZip;
  module CTNode = Core_Tree_Node;
};