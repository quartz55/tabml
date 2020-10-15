open Globals;

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

let getAllTabs = () =>
  RJs.Promise.toIOLazy(() => B.Tabs.(query(makeQueryObj())))
  |> IO.mapError(e => `BrowserError(e));

let getAllWindows = () =>
  RJs.Promise.toIOLazy(() => B.Windows.getAll())
  |> IO.mapError(e => `BrowserError(e));
