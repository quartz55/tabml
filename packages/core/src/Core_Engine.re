open Globals;
module Store = Core_Store;
module Tree = Core_Tree;
module Client = Core_Client;

module BAPI = {
  module Error = {
    type t = [ | `BrowserError(Js.Promise.error)];
    let make = e => `BrowserError(e);
  };
  module IOE = IO.WithError(Error);

  let activateWindow = winId =>
    RJs.Promise.toIOLazy(() =>
      BWindows.(update(winId, makeUpdateData(~focused=true, ())))
    )
    |> IOE.mapError(Error.make);

  let activateTab = (~window=?, tabId) => {
    let win =
      Option.Infix.(activateWindow >> IO.map(ignore) <$> window |? IO.pure());
    let tab =
      RJs.Promise.toIOLazy(() =>
        BTabs.(update(tabId, makeUpdateProps(~active=true, ())))
      )
      |> IOE.mapError(Error.make);
    IOE.map2((_, tab) => tab, win, tab);
  };

  let removeTab = tabId =>
    RJs.Promise.toIOLazy(() => BTabs.(remove(tabId)))
    |> IOE.mapError(Error.make);

  let removeWindow = winId =>
    RJs.Promise.toIOLazy(() => BWindows.(remove(winId)))
    |> IOE.mapError(Error.make);

  let setTabParent = (parentId, tabId) =>
    RJs.Promise.toIOLazy(() =>
      BTabs.(update(tabId, makeUpdateProps(~openerTabId=parentId, ())))
    )
    |> IOE.mapError(Error.make);
};

type state = {session: Tree.t};

module BrowserAction = {
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
};

module ClientAction = {
  module Error = {
    type t = [ | `BrowserError(Js.Promise.error) | `ClientError(string)];
    let client: Logger.msgf('a) => t =
      msgf => `ClientError(msgf(fmt => Format.asprintf(fmt ^^ "")));

    let show =
      fun
      | `BrowserError(e) =>
        Format.sprintf("browser error: %s", Js.String.make(e))
      | `ClientError(e) => Format.sprintf("client error: %s", e);
  };

  module IOE = {
    include IO.WithError(Error);
    let let_ = bind;
  };

  let logPathError = (tree, path, ()) => {
    Logger.err(m =>
      m(
        "Couldn't find node at %s\n%s",
        Tree.showPath(path),
        Tree.show(tree),
      )
    );
    Store.NoUpdate;
  };

  let reducer = (state, action: Client.Action.t) => {
    let tree = state.session;

    switch (action) {
    | Activate(path) =>
      Logger.info(m => m("Activating node at %s", Tree.showPath(path)));
      Tree.getNodeAt(path, tree)
      |> Option.map(node =>
           Store.IO(
             _ =>
               switch (Tree.Node.type_(node)) {
               | Tab(t) =>
                 BAPI.activateTab(
                   ~window=t.windowId,
                   Option.getOrThrow(t.id),
                 )
                 |> IO.map(ignore)
               | Window(w) =>
                 BAPI.activateWindow(Option.getOrThrow(w.id))
                 |> IO.map(ignore)
               | Session(_) => IO.pure()
               },
           )
         )
      |> Option.getOrElseLazy(logPathError(tree, path));

    | Remove(path) =>
      Logger.info(m => m("Removing node at %s", Tree.showPath(path)));
      Tree.getNodeAt(path, tree)
      |> Option.map(node =>
           Store.IO(
             _ =>
               switch (Tree.Node.type_(node)) {
               | Tab(t) => BAPI.removeTab(Option.getOrThrow(t.id))
               | Window(w) => BAPI.removeWindow(Option.getOrThrow(w.id))
               | Session(_) => IO.pure()
               },
           )
         )
      |> Option.getOrElseLazy(logPathError(tree, path));

    | ToggleCollapse(path) =>
      Logger.info(m =>
        m("Toggling collapse state of node at %s", Tree.showPath(path))
      );
      Tree.(mapNodeAt(Node.toggleCollapsed, path, tree))
      |> Option.map(tree => Store.Update({session: tree}))
      |> Option.getOrElseLazy(logPathError(tree, path));
    };
  };
};

type action =
  | Browser(BrowserAction.t)
  | Client(Client.Action.t);

let reducer = (state, action) => {
  switch (action) {
  | Browser(action) => BrowserAction.reducer(state, action)
  | Client(action) => ClientAction.reducer(state, action)
  };
};

let addClient = (client, clients) => List.cons(client, clients);

let removeClient = (client, clients) =>
  List.reject(Client.equal(client), clients);

let broadcastToClients = (state, clients) =>
  clients
  |> List.forEach(client =>
       try(Client.send(state, client)) {
       | Js.Exn.Error(e) =>
         Logger.err(m =>
           m(
             "error sending updated state to client %a: %s",
             Client.pp,
             client,
             Js.String.make(e),
           )
         )
       }
     );

let start = () => {
  let clients = ref([]);

  // let reducer = (s, a) =>
  //   try(reducer(s, a)) {
  //   | Failure(msg) => IO.throw(`DispatchError(msg))
  //   | Js.Exn.Error(e) => IO.throw(`DispatchError(Js.String.make(e)))
  //   | e => IO.throw(`DispatchError(Printexc.to_string(e)))
  //   };

  let store =
    Store.make(
      ~reducer,
      ~backend=Store.Backends.ref_(),
      ~init=() => Tree.bootstrap() |> IO.map(session => {session: session}),
      ~sub=state => broadcastToClients(state.session, clients^),
    );

  let dispatch = a =>
    Store.dispatch(a, store)
    |> IO.unsafeRunAsync(
         fun
         | Ok () => ()
         | Error(e) => {
             let msg =
               switch (e) {
               | `BrowserError(e) => "browser error" ++ Js.String.make(e)
               //  | `DispatchError(e) => "dispatch error" ++ e
               };
             Logger.err(m =>
               m("error dispatching action %s: %s", Js.String.make(a), msg)
             );
           },
       );

  // Frontend handling
  B.Runtime.onConnect
  |> EL.addListener(p => {
       let client = Client.make(p);
       clients := addClient(client, clients^);
       p.onDisconnect
       |> EL.addListener(_ => clients := removeClient(client, clients^));
       p.onMessage |> EL.addListener(a => dispatch(Client(a)));
     });

  let dispatchBrowser = a => dispatch(Browser(a));

  // Window handling
  B.Windows.onCreated
  |> EL.addListener(win => dispatchBrowser(CreateWindow(win)));
  B.Windows.onFocusChanged
  |> EL.addListener(winId => dispatchBrowser(ActivateWindow(winId)));
  B.Windows.onRemoved
  |> EL.addListener(winId => dispatchBrowser(RemoveWindow(winId)));

  // Tab handling
  B.Tabs.onCreated |> EL.addListener(tab => dispatchBrowser(CreateTab(tab)));
  B.Tabs.onReplaced
  |> EL.addListener((~added, removed) =>
       dispatchBrowser(ReplaceTab(added, removed))
     );
  B.Tabs.onAttached
  |> EL.addListener((tabId, ai) =>
       dispatchBrowser(
         AttachTab(tabId, ai.BTabs.newWindowId, ai.newPosition),
       )
     );
  B.Tabs.onMoved
  |> EL.addListener((tabId, BTabs.{windowId, fromIndex, toIndex}) =>
       dispatchBrowser(MoveTab(tabId, windowId, (fromIndex, toIndex)))
     );
  B.Tabs.onRemoved
  |> EL.addListener(tabId => dispatchBrowser(RemoveTab(tabId)));
  B.Tabs.onActivated
  |> EL.addListener(info => dispatchBrowser(ActivateTab(info)));
  B.Tabs.onUpdated
  |> EL.addListener((tabId, changes, tab) =>
       dispatchBrowser(UpdateTab(tabId, changes, tab))
     );
};