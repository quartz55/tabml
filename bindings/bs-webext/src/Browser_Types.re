module EventListener = {
  type t('a);

  [@bs.send.pipe: t('a)] external addListener: 'a => unit = "addListener";

  [@bs.send.pipe: t('a)] external hasListener: 'a => bool = "hasListener";

  [@bs.send.pipe: t('a)]
  external removeListener: 'a => unit = "removeListener";
};

module Port = {
  type t('send, 'recv) = {
    name: string,
    error: option(string),
    onDisconnect: EventListener.t(t('send, 'recv) => unit),
    onMessage: EventListener.t('recv => unit),
  };

  [@bs.send.pipe: t('send, 'recv)] external disconnect: unit = "disconnect";

  [@bs.send.pipe: t('send, 'recv)]
  external postMessage: 'send => unit = "postMessage";
};

type tabId = int;
type windowId = int;
type sessionId = string;

module TabStatus: {
  type t;
  type view =
    | Loading
    | Complete;

  [@bs.inline "loading"]
  let loading: t;
  [@bs.inline "complete"]
  let complete: t;

  let view: t => view;
} = {
  type t = string;
  type view =
    | Loading
    | Complete;

  [@bs.inline]
  let loading = "loading";
  [@bs.inline]
  let complete = "complete";

  let view =
    fun
    | "loading" => Loading
    | "complete" => Complete
    | other => failwith("unrecognized TabStatus value: " ++ other);
};

module Tab = {
  type t = {
    active: bool,
    attention: option(bool),
    audible: option(bool),
    discarded: option(bool),
    favIconUrl: option(string),
    hidden: bool,
    highlighted: bool,
    id: option(tabId),
    index: int,
    openerTabId: option(tabId),
    pinned: bool,
    status: option(TabStatus.t),
    successorId: option(tabId),
    title: option(string),
    url: option(string),
    windowId,
  };
};

module WindowState: {
  type t;
  type view =
    | Normal
    | Minimized
    | Maximized
    | Fullscreen
    | Docked;

  [@bs.inline "normal"]
  let normal: t;
  [@bs.inline "minimized"]
  let minimized: t;
  [@bs.inline "maximized"]
  let maximized: t;
  [@bs.inline "fullscreen"]
  let fullscreen: t;
  [@bs.inline "docked"]
  let docked: t;

  let view: t => view;
} = {
  type t = string;
  type view =
    | Normal
    | Minimized
    | Maximized
    | Fullscreen
    | Docked;

  [@bs.inline]
  let normal = "normal";
  [@bs.inline]
  let minimized = "minimized";
  [@bs.inline]
  let maximized = "maximized";
  [@bs.inline]
  let fullscreen = "fullscreen";
  [@bs.inline]
  let docked = "docked";

  let view =
    fun
    | "normal" => Normal
    | "minimized" => Minimized
    | "maximized" => Maximized
    | "fullscreen" => Fullscreen
    | "docked" => Docked
    | other => failwith("unrecognized WindowState value: " ++ other);
};

module WindowType: {
  type t;
  type view =
    | Normal
    | Popup
    | Panel
    | Devtools;

  [@bs.inline "normal"]
  let normal: t;
  [@bs.inline "popup"]
  let popup: t;
  [@bs.inline "panel"]
  let panel: t;
  [@bs.inline "devtools"]
  let devtools: t;

  let view: t => view;
} = {
  type t = string;
  type view =
    | Normal
    | Popup
    | Panel
    | Devtools;

  [@bs.inline]
  let normal = "normal";
  [@bs.inline]
  let popup = "popup";
  [@bs.inline]
  let panel = "panel";
  [@bs.inline]
  let devtools = "devtools";

  let view =
    fun
    | "normal" => Normal
    | "popup" => Popup
    | "panel" => Panel
    | "devtools" => Devtools
    | other => failwith("unrecognized WindowType value: " ++ other);
};

module Window = {
  type t = {
    alwaysOnTop: bool,
    focused: bool,
    height: option(int),
    id: option(windowId),
    incognito: bool,
    left: option(int),
    sessionId: option(sessionId),
    state: option(WindowState.t),
    tabs: array(Tab.t),
    title: option(string),
    top: option(int),
    [@bs.as "type"]
    type_: option(WindowType.t),
    width: option(int),
  };
};

module StorageArea = {
  type t;
  type res('a) = Js.Dict.t('a);

  [@bs.send.pipe: t] external get: string => Js.Promise.t(res('a)) = "get";

  [@bs.send.pipe: t]
  external getMany: array(string) => Js.Promise.t(res('a)) = "get";

  [@bs.send.pipe: t] external getAll: unit => Js.Promise.t(res('a)) = "get";

  [@bs.send.pipe: t] external set: res('a) => Js.Promise.t(unit) = "set";

  [@bs.send.pipe: t] external remove: string => Js.Promise.t(unit) = "remove";

  [@bs.send.pipe: t]
  external removeMany: array(string) => Js.Promise.t(unit) = "remove";

  [@bs.send.pipe: t] external clear: unit => Js.Promise.t(unit) = "clear";
};