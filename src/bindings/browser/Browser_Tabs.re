open Relude.Globals;
open Browser_Types;

type queryObj;
[@bs.obj]
external makeQueryObj:
  (
    ~active: bool=?,
    ~audible: bool=?,
    ~currentWindow: bool=?,
    ~discarded: bool=?,
    ~hidden: bool=?,
    ~highlighted: bool=?,
    ~index: int=?,
    ~muted: bool=?,
    ~pinned: bool=?,
    ~status: string=?,
    ~title: string=?,
    ~url: string=?,
    ~windowId: windowId=?,
    unit
  ) =>
  queryObj;

[@bs.val] [@bs.scope ("browser", "tabs")]
external query: queryObj => Js.Promise.t(array(Tab.t)) = "query";
let query = q => Relude.Js.Promise.toIOLazy(() => query(q));

[@bs.val] [@bs.scope ("browser", "tabs")]
external get: tabId => Js.Promise.t(Tab.t) = "get";
let get = id => Relude.Js.Promise.toIOLazy(() => get(id));

type moveProps;
[@bs.obj] external makeMoveProps: unit => moveProps;
[@bs.val] [@bs.scope ("browser", "tabs")]
external move: (tabId, moveProps) => Js.Promise.t(Tab.t) = "move";
let move = (id, props) => Relude.Js.Promise.toIOLazy(() => move(id, props));

[@bs.val] [@bs.scope ("browser", "tabs")]
external moveMany: (array(tabId), moveProps) => Js.Promise.t(array(Tab.t)) =
  "move";
let moveMany = (ids, props) =>
  Relude.Js.Promise.toIOLazy(() => moveMany(Array.fromList(ids), props));

[@bs.val] [@bs.scope ("browser", "tabs")]
external reload: tabId => Js.Promise.t(unit) = "reload";
let reload = id => Relude.Js.Promise.toIOLazy(() => reload(id));

[@bs.val] [@bs.scope ("browser", "tabs")]
external remove: tabId => Js.Promise.t(unit) = "remove";
let remove = id => Relude.Js.Promise.toIOLazy(() => remove(id));

[@bs.val] [@bs.scope ("browser", "tabs")]
external removeMany: array(tabId) => Js.Promise.t(unit) = "remove";
let removeMany = ids =>
  Relude.Js.Promise.toIOLazy(() => removeMany(Array.fromList(ids)));

type activeInfo = {
  previousTabId: option(tabId),
  tabId,
  windowId,
};

[@bs.val] [@bs.scope ("browser", "tabs")]
external onActivated: EventListener.t(activeInfo => unit) = "onActivated";

type attachInfo = {
  newWindowId: windowId,
  newPosition: int,
};

[@bs.val] [@bs.scope ("browser", "tabs")]
external onAttached: EventListener.t((tabId, attachInfo) => unit) =
  "onAttached";

[@bs.val] [@bs.scope ("browser", "tabs")]
external onCreated: EventListener.t(Tab.t => unit) = "onCreated";

[@bs.val] [@bs.scope ("browser", "tabs")]
external onDetached: EventListener.t(activeInfo => unit) = "onDetached";

type moveInfo = {
  windowId,
  fromIndex: int,
  toIndex: int,
};
[@bs.val] [@bs.scope ("browser", "tabs")]
external onMoved: EventListener.t((tabId, moveInfo) => unit) = "onMoved";

[@bs.val] [@bs.scope ("browser", "tabs")]
external onRemoved: EventListener.t(tabId => unit) = "onRemoved";

[@bs.val] [@bs.scope ("browser", "tabs")]
external onReplaced: EventListener.t((~added: tabId, tabId) => unit) =
  "onReplaced";

type changeInfo = {
  attention: option(bool),
  audible: option(bool),
  discarded: option(bool),
  favIconUrl: option(string),
  hidden: option(bool),
  isArticle: option(bool),
  pinned: option(bool),
  status: option(string),
  title: option(string),
  url: option(string),
};
[@bs.val] [@bs.scope ("browser", "tabs")]
external onUpdated: EventListener.t((int, changeInfo, Tab.t) => unit) =
  "onUpdated";