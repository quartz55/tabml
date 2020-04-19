open Browser_Types;

[@bs.val] [@bs.scope ("browser", "tabs")]
external _TAB_ID_NONE: tabId = "TAB_ID_NONE";
let c_TAB_ID_NONE = _TAB_ID_NONE;

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
    ~status: TabStatus.t=?,
    ~title: string=?,
    ~url: string=?,
    ~windowId: windowId=?,
    unit
  ) =>
  queryObj;

[@bs.val] [@bs.scope ("browser", "tabs")]
external query: queryObj => Js.Promise.t(array(Tab.t)) = "query";

[@bs.val] [@bs.scope ("browser", "tabs")]
external get: tabId => Js.Promise.t(Tab.t) = "get";

type moveProps;
[@bs.obj] external makeMoveProps: unit => moveProps;
[@bs.val] [@bs.scope ("browser", "tabs")]
external move: (tabId, moveProps) => Js.Promise.t(Tab.t) = "move";

[@bs.val] [@bs.scope ("browser", "tabs")]
external moveMany: (array(tabId), moveProps) => Js.Promise.t(array(Tab.t)) =
  "move";

[@bs.val] [@bs.scope ("browser", "tabs")]
external reload: tabId => Js.Promise.t(unit) = "reload";

[@bs.val] [@bs.scope ("browser", "tabs")]
external remove: tabId => Js.Promise.t(unit) = "remove";

[@bs.val] [@bs.scope ("browser", "tabs")]
external removeMany: array(tabId) => Js.Promise.t(unit) = "remove";

type updateProps;
[@bs.obj]
external makeUpdateProps:
  (
    ~active: bool=?,
    ~autoDiscardable: bool=?,
    ~highlighted: bool=?,
    ~loadReplace: bool=?,
    ~muted: bool=?,
    ~openerTabId: tabId=?,
    ~pinned: bool=?,
    ~successorTabId: tabId=?,
    ~url: string=?,
    unit
  ) =>
  updateProps;
[@bs.val] [@bs.scope ("browser", "tabs")]
external update: (tabId, updateProps) => Js.Promise.t(Tab.t) = "update";

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
  status: option(TabStatus.t),
  title: option(string),
  url: option(string),
};
[@bs.val] [@bs.scope ("browser", "tabs")]
external onUpdated: EventListener.t((int, changeInfo, Tab.t) => unit) =
  "onUpdated";