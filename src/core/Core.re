open Globals;
module Tree = Core_Tree;
module Store = Core_Store;

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
  | FullUpdate(Tree.t);

let reducer = (state, action) =>
  switch (action) {
  | CreateWindow(win) =>
    Logger.info(m => m("creating window %s", stringifyExn(win)));
    state
    |> Tree.findWindowWithId(BWindow.id(win))
    |> Option.map(wz =>
         wz
         |> Tree.TZip.modifyFocusValue(Tree.Node.updateWindow(const(win)))
         |> Tree.rebuild
       )
    |> Option.getOrElseLazy(() =>
         state |> Tree.appendWindow(Tree.Node.ofWindow(win))
       );
  | ActivateWindow(winId) =>
    Logger.info(m => m("activating window %d", winId));
    state
    |> Tree.mapNode(n =>
         switch (n) {
         | Window({focused: true, _} as w) =>
           Some(Window({...w, focused: false}))
         | _ => None
         }
       )
    |> Tree.modifyWindowWithId(w => {...w, focused: true}, winId);
  | RemoveWindow(winId) =>
    Logger.info(m => m("removing window %d", winId));
    state |> Tree.removeWindowWithId(winId);
  | CreateTab(tab) =>
    Logger.info(m => m("creating tab %s", stringifyExn(tab)));
    state
    |> Tree.addCreatedTab(tab)
    |> Option.tapNone(() => Logger.err(m => m("tab wasn't created?")))
    |> Option.getOrThrow;
  // TODO
  | ReplaceTab(_added, _removed) =>
    failwith("tab replacement not implemented yet!! sorry :(")
  | AttachTab(tabId, winId, idx) =>
    Logger.info(m =>
      m("attaching tab %d to window %d at idx %d", tabId, winId, idx)
    );
    Tree.moveTabWithIdToIdx(~window=`OfId(winId), tabId, idx, state)
    |> Option.getOrThrow;
  | MoveTab(tabId, winId, (fromIdx, toIdx)) =>
    Logger.info(m =>
      m(
        "moving tab %d on window %d from idx %d to %d",
        tabId,
        winId,
        fromIdx,
        toIdx,
      )
    );
    Tree.moveTabWithIdToIdx(tabId, toIdx, state) |> Option.getOrThrow;
  | RemoveTab(tabId) =>
    Logger.info(m => m("removing tab %d", tabId));
    state |> Tree.removeTabWithId(tabId);
  | ActivateTab({previousTabId, tabId, _}) =>
    Logger.info(m => m("activate tab %d", tabId));
    previousTabId
    |> Option.map(tabId => {
         Logger.info(m => m("deactivating %d first", tabId));
         state |> Tree.modifyTabWithId(t => {...t, active: false}, tabId);
       })
    |> Option.getOrElse(state)
    |> Tree.modifyTabWithId(t => {...t, active: true}, tabId);
  | UpdateTab(tabId, changes, newTab) =>
    Logger.info(m =>
      m(
        "updating tab %d\nchanges: %s\nnewtab: %s",
        tabId,
        stringifyExn(changes),
        stringifyExn(newTab),
      )
    );
    state |> Tree.setTabWithId(tabId, newTab);
  | FullUpdate(tree) =>
    Logger.warn(m => m("full tree update"));
    tree;
  };

let run = () => {
  let store =
    Store.make(
      ~init=
        Tree.bootstrap
        >> IO.mapError([%raw "e => 'bootstrap error: ' + e.toString()"]),
      ~reducer=
        (state, action) =>
          try(reducer(state, action) |> IO.pure) {
          | Failure(msg) => IO.throw(msg)
          | Js.Exn.Error(e) => IO.throw(Js.String.make(e))
          | e => IO.throw(Printexc.to_string(e))
          },
      ~backend=Store.Backends.ref_(),
    );

  let subs = ref([]);
  let dispatch = action => {
    store
    |> Store.dispatch(action)
    |> IO.unsafeRunAsync(
         fun
         | Ok(state) =>
           subs^
           |> List.forEach(s =>
                try(BPort.postMessage(s, state)) {
                | Js.Exn.Error(e) =>
                  Logger.err(m =>
                    m("error posting message: %s", Js.String.make(e))
                  )
                }
              )
         | Error(msg) => {
             Logger.err(m => m("ahhhh"));
             Js.Console.error2("error dispatching:", msg);
           },
       );
  };

  let handleClient = port => {
    Store.get(store)
    |> IO.unsafeRunAsync(s =>
         switch (s) {
         | Ok(s) =>
           BPort.postMessage(port, s);
           port.onDisconnect
           |> EL.addListener(_p => {
                subs := subs^ |> List.reject([%raw "x => x == port"])
              });
           subs := [port, ...subs^];
         | Error(msg) => Js.Console.error2("error getting state:", msg)
         }
       );
  };

  let _rebuild = _ =>
    Tree.bootstrap()
    |> IO.unsafeRunAsync(
         fun
         | Ok(t) => dispatch(FullUpdate(t))
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
  Browser.Tabs.onCreated |> EL.addListener(tab => dispatch(CreateTab(tab)));
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

  // Frontend handling
  Browser.Runtime.onConnect |> EL.addListener(handleClient);
};

module Globals = {
  module CTree = Core_Tree;
  module CTZip = Core_Tree.TZip;
  module CTNode = Core_Tree_Node;
};