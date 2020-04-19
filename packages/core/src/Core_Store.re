open Globals;

module Backend = {
  type t('state, 'error) = (getfn('state, 'error), setfn('state, 'error))
  and getfn('state, 'error) = unit => IO.t(option('state), 'error)
  and setfn('state, 'error) = 'state => IO.t(unit, 'error);

  let makeLabels:
    (~get: getfn('state, 'error), ~set: setfn('state, 'error)) =>
    t('state, 'error) =
    (~get, ~set) => (get, set);
  let make = (get, set) => makeLabels(~get, ~set);
  let get: t('state, 'error) => IO.t(option('state), 'error) =
    ((get, _)) => get();
  let set: ('state, t('state, 'error)) => IO.t(unit, 'error) =
    (v, (_, set)) => set(v);
};

type t('state, 'action, 'error) = {
  backend: Backend.t('state, 'error),
  reducer: reducer('state, 'action, 'error),
  init: unit => IO.t('state, 'error),
  queue: array('action),
  sub: 'state => unit,
}
and reducer('state, 'action, 'error) =
  ('state, 'action) => update('action, 'state, 'error)
and update('action, 'state, 'error) =
  | NoUpdate
  | Update('state)
  | IO(dispatchfn('action) => IO.t(unit, 'error))
  | UpdateIO('state, dispatchfn('action) => IO.t(unit, 'error))
and dispatchfn('action) = 'action => unit;

let make = (~reducer, ~init, ~backend, ~sub) => {
  backend,
  reducer,
  init,
  queue: [||],
  sub,
};

let set: ('state, t('state, 'action, 'error)) => IO.t(unit, 'error) =
  (state, {backend, sub, _}) =>
    Backend.set(state, backend)
    |> IO.tap(() => Logger.info(m => m("set")))
    |> IO.tap(() => sub(state));

let get: t('state, 'action, 'error) => IO.t('state, 'error) =
  ({init, backend, _} as t) =>
    Backend.get(backend)
    |> IO.tap(_ => Logger.info(m => m("get")))
    |> IO.flatMap(
         fun
         | None => {
             Logger.info(m => m("state wasn't set yet, calling init"));
             init()
             |> IO.flatMap(state => set(state, t) |> IO.map(const(state)));
           }
         | Some(state) => IO.pure(state),
       );

let dispatch: ('action, t('state, 'action, 'error)) => IO.t(unit, 'error) =
  (action, {reducer, _} as t) =>
    get(t)
    |> IO.flatMap(state =>
         switch (reducer(state, action)) {
         | NoUpdate => IO.pure()
         | Update(state) => set(state, t)
         | IO(fn) => fn(_ => ())
         | UpdateIO(state, fn) =>
           set(state, t) |> IO.flatMap(_ => fn(_ => ()))
         }
       );

module Backends = {
  let localStorage = key => {
    let s = B.Storage.local;
    let get = () =>
      RJs.Promise.toIOLazy(() => s |> BT.StorageArea.get(key))
      |> IO.map(obj => Js.Dict.get(obj, key))
      |> IO.mapError(e => `BrowserError(e));
    //  [%raw "err => 'localStorageGetError: ' + err.toString()"],

    let set = v =>
      RJs.Promise.toIOLazy(() =>
        s |> BT.StorageArea.set(Js.Dict.fromList([(key, v)]))
      )
      |> IO.mapError(e => `BrowserError(e));
    //  [%raw "err => 'localStorageSetError: ' + err.toString()"],

    Backend.make(get, set);
  };

  let ref_ = () => {
    let s = ref(None);
    let get = () => IO.pure(s^);
    let set = v => {
      s := Some(v);
      IO.pure();
    };
    Backend.make(get, set);
  };
};