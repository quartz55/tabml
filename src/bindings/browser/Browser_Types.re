module EventListener = {
  type t('a);

  [@bs.send.pipe: t('a)] external addListener: 'a => unit = "addListener";

  [@bs.send.pipe: t('a)] external hasListener: 'a => bool = "hasListener";

  [@bs.send.pipe: t('a)]
  external removeListener: 'a => unit = "removeListener";
};

module Port = {
  type t('msg) = {
    name: string,
    error: option(string),
    onDisconnect: EventListener.t(t('msg) => unit),
    onMessage: EventListener.t('msg => unit),
  };

  [@bs.send] external disconnect: t('msg) => unit = "disconnect";
  [@bs.send] external postMessage: (t('msg), 'msg) => unit = "postMessage";
};

type tabId = int;
type windowId = int;

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
    status: option(string),
    successorId: option(tabId),
    title: option(string),
    url: option(string),
    windowId,
  };

  let idOpt = ({id, _}) => id;
  let id = t => idOpt(t) |> Belt.Option.getExn;
};

module Window = {
  type t = {
    alwaysOnTop: bool,
    focused: bool,
    id: option(windowId),
    tabs: array(Tab.t),
    title: option(string),
  };

  let placeholder = id => {
    alwaysOnTop: false,
    focused: false,
    id: Some(id),
    tabs: [||],
    title: None,
  };

  let idOpt = ({id, _}) => id;
  let id = t => idOpt(t) |> Belt.Option.getExn;
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