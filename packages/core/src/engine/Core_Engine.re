open Globals;
module Types = Core_Engine_Types;
open Types;
module Store = Core_Store;
module Tree = Core_Tree;
module Client = Core_Client;
module BAPI = Core_BAPI;
module BrowserAction = Core_Engine_BrowserAction;
module ClientAction = Core_Engine_ClientAction;

type action =
  | Browser(BrowserAction.t)
  | Client(ClientAction.t);

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
