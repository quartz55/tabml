open Browser_Types;

[@bs.val] [@bs.scope ("browser", "windows")]
external get: int => Js.Promise.t(Window.t) = "get";
let get = id => Relude.Js.Promise.toIOLazy(() => get(id));

[@bs.val] [@bs.scope ("browser", "windows")]
external getAll: unit => Js.Promise.t(array(Window.t)) = "getAll";
let getAll = () => Relude.Js.Promise.toIOLazy(() => getAll());

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
    ~state: string=?,
    ~tabId: int=?,
    ~titlePreface: string=?,
    ~top: int=?,
    ~_type: string=?,
    ~url: string=?,
    ~width: int=?,
    unit
  ) =>
  createData;

[@bs.val] [@bs.scope ("browser", "windows")]
external create: createData => Js.Promise.t(Window.t) = "create";

type updateData;
[@bs.obj]
external makeUpdateData:
  (
    ~drawAttention: bool=?,
    ~focused: bool=?,
    ~height: int=?,
    ~incognito: bool=?,
    ~left: int=?,
    ~state: string=?,
    ~titlePreface: string=?,
    ~top: int=?,
    ~width: int=?,
    unit
  ) =>
  updateData;

[@bs.val] [@bs.scope ("browser", "windows")]
external update: (int, updateData) => Js.Promise.t(Window.t) = "update";

[@bs.val] [@bs.scope ("browser", "windows")]
external onCreated: EventListener.t(Window.t => unit) = "onCreated";

[@bs.val] [@bs.scope ("browser", "windows")]
external onRemoved: EventListener.t(int => unit) = "onRemoved";

[@bs.val] [@bs.scope ("browser", "windows")]
external onFocusChanged: EventListener.t(int => unit) = "onFocusChanged";