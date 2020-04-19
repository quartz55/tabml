open Browser_Types;

[@bs.val] [@bs.scope ("browser", "windows")]
external _WINDOW_ID_NONE: windowId = "WINDOW_ID_NONE";
let c_WINDOW_ID_NONE = _WINDOW_ID_NONE;

[@bs.val] [@bs.scope ("browser", "windows")]
external _WINDOW_ID_CURRENT: windowId = "WINDOW_ID_CURRENT";
let c_WINDOW_ID_CURRENT = _WINDOW_ID_CURRENT;

[@bs.val] [@bs.scope ("browser", "windows")]
external get: int => Js.Promise.t(Window.t) = "get";

[@bs.val] [@bs.scope ("browser", "windows")]
external getAll: unit => Js.Promise.t(array(Window.t)) = "getAll";

type createData;
[@bs.obj]
external makeCreateData:
  (
    ~allowScriptsToClose: bool=?,
    ~cookieStoreId: int=?,
    ~focused: bool=?,
    ~height: int=?,
    ~incognito: bool=?,
    ~left: int=?,
    ~state: WindowState.t=?,
    ~tabId: int=?,
    ~titlePreface: string=?,
    ~top: int=?,
    ~_type: [@bs.string] [ | `normal | `popup | `panel | `detached_panel]=?,
    ~url: string=?,
    ~width: int=?,
    unit
  ) =>
  createData;

[@bs.val] [@bs.scope ("browser", "windows")]
external create: createData => Js.Promise.t(Window.t) = "create";

[@bs.val] [@bs.scope ("browser", "windows")]
external remove: windowId => Js.Promise.t(unit) = "remove";

type updateData;
[@bs.obj]
external makeUpdateData:
  (
    ~drawAttention: bool=?,
    ~focused: bool=?,
    ~height: int=?,
    ~incognito: bool=?,
    ~left: int=?,
    ~state: WindowState.t=?,
    ~titlePreface: string=?,
    ~top: int=?,
    ~width: int=?,
    unit
  ) =>
  updateData;

[@bs.val] [@bs.scope ("browser", "windows")]
external update: (windowId, updateData) => Js.Promise.t(Window.t) = "update";

[@bs.val] [@bs.scope ("browser", "windows")]
external onCreated: EventListener.t(Window.t => unit) = "onCreated";

[@bs.val] [@bs.scope ("browser", "windows")]
external onRemoved: EventListener.t(windowId => unit) = "onRemoved";

[@bs.val] [@bs.scope ("browser", "windows")]
external onFocusChanged: EventListener.t(windowId => unit) = "onFocusChanged";