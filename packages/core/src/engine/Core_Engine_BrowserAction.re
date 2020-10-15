open Globals;
module Types = Core_Engine_Types;
open Types;
module Store = Core_Store;
module Tree = Core_Tree;
module Client = Core_Client;
module BAPI = Core_BAPI;

type t =
  | CreateWindow(BWindow.t)
  | ActivateWindow(BT.windowId)
  | RemoveWindow(BT.windowId)
  | CreateTab(BTab.t)
  | ReplaceTab(BT.tabId, BT.tabId)
  | AttachTab(BT.tabId, int, int)
  | MoveTab(BT.tabId, BT.windowId, (int, int))
  | RemoveTab(BT.tabId)
  | ActivateTab(BTabs.activeInfo)
  | UpdateTab(BT.tabId, BTabs.changeInfo, BTab.t);

let reducer = (state, action) => {
  let tree = state.session;
  switch (action) {
  | CreateWindow(win) =>
    Logger.info(m => m("creating window %s", stringifyExn(win)));
    let tree =
      tree
      |> Tree.findWindowWithId(Option.getOrThrow(win.id))
      |> Option.map(
           Tree.TZip.modifyFocusValue(Tree.Node.updateWindow(const(win)))
           >> Tree.rebuild,
         )
      |> Option.getOrElseLazy(() =>
           tree |> Tree.appendWindow(Tree.Node.ofWindow(win))
         );
    Store.Update({session: tree});

  | ActivateWindow(winId) =>
    Logger.info(m => m("activating window %d", winId));
    let tree =
      tree
      |> Tree.mapNode(n =>
           switch (n) {
           | Window({focused: true, _} as w) =>
             Some(Window({...w, focused: false}))
           | _ => None
           }
         )
      |> Tree.modifyWindowWithId(w => {...w, focused: true}, winId);
    Store.Update({session: tree});

  | RemoveWindow(winId) =>
    Logger.info(m => m("removing window %d", winId));
    let tree = tree |> Tree.removeWindowWithId(winId);
    Store.Update({session: tree});

  | CreateTab(tab) =>
    Logger.info(m => m("creating tab %s", stringifyExn(tab)));
    let tree =
      tree
      |> Tree.addCreatedTab(tab)
      |> Option.tapNone(() => Logger.err(m => m("tab wasn't created?")))
      |> Option.getOrThrow;
    Store.Update({session: tree});

  | ReplaceTab(_added, _removed) =>
    // TODO Handle ReplaceTab event (#chrome)
    failwith("tab replacement not implemented yet!! sorry chrome users :(")

  | AttachTab(tabId, winId, idx) =>
    Logger.info(m =>
      m("attaching tab %d to window %d at idx %d", tabId, winId, idx)
    );
    let (t, newParent) =
      Tree.moveTabWithIdToIdx(~window=`OfId(winId), tabId, idx, tree)
      |> Option.getOrThrow;
    switch (newParent) {
    | Some(id) =>
      Store.UpdateIO(
        {session: t},
        _ => BAPI.setTabParent(id, tabId) |> IO.map(const()),
      )
    | _ => Store.Update({session: t})
    };

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
    let (t, newParent) =
      Tree.moveTabWithIdToIdx(tabId, toIdx, tree) |> Option.getOrThrow;
    switch (newParent) {
    | Some(id) =>
      Store.UpdateIO(
        {session: t},
        _ => BAPI.setTabParent(id, tabId) |> IO.map(const()),
      )
    | _ => Store.Update({session: t})
    };

  | RemoveTab(tabId) =>
    Logger.info(m => m("removing tab %d", tabId));
    Store.Update({session: tree |> Tree.removeTabWithId(tabId)});

  | ActivateTab({previousTabId, tabId, _}) =>
    Logger.info(m => m("activate tab %d", tabId));
    let tree =
      previousTabId
      |> Option.map(tabId => {
           Logger.info(m => m("deactivating %d first", tabId));
           tree |> Tree.modifyTabWithId(t => {...t, active: false}, tabId);
         })
      |> Option.getOrElse(tree)
      |> Tree.modifyTabWithId(t => {...t, active: true}, tabId);
    Store.Update({session: tree});

  | UpdateTab(tabId, changes, newTab) =>
    Logger.info(m =>
      m(
        "updating tab %d\nchanges: %s\nnewtab: %s",
        tabId,
        stringifyExn(changes),
        stringifyExn(newTab),
      )
    );
    Store.Update({session: tree |> Tree.setTabWithId(tabId, newTab)});
  };
};
