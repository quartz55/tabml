open Globals;

module Backend = {
  type t('state) = (getfn('state), setfn('state))
  and getfn('state) = unit => IO.t(option('state), string)
  and setfn('state) = 'state => IO.t(unit, string);

  let makeLabels: (~get: getfn('state), ~set: setfn('state)) => t('state) =
    (~get, ~set) => (get, set);
  let make = (get, set) => makeLabels(~get, ~set);
  let get: t('state) => IO.t(option('state), string) =
    ((get, _)) => get();
  let set: ('state, t('state)) => IO.t(unit, string) =
    (v, (_, set)) => set(v);
};
type t('state, 'action) = {
  backend: Backend.t('state),
  reducer: ('state, 'action) => IO.t('state, string),
  init: unit => IO.t('state, string),
};

let make = (~init, ~reducer, ~backend) => {backend, reducer, init};

let set: ('state, t('state, 'action)) => IO.t(unit, string) =
  (state, {backend, _}) =>
    Backend.set(state, backend) |> IO.tap(() => Logger.info(m => m("set")));

let get: t('state, 'action) => IO.t('state, string) =
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

let dispatch: ('action, t('state, 'action)) => IO.t('state, string) =
  (action, {reducer, _} as t) =>
    get(t)
    |> IO.flatMap(state => reducer(state, action))
    |> IO.flatMap(state => {
         Logger.debug(m =>
           m("dispatched successfully, setting\n%s", Core_Tree.show(state))
         );
         set(state, t) |> IO.map(const(state));
       });

module Backends = {
  let localStorage = key => {
    let s = B.Storage.local;
    let get = () =>
      Relude.Js.Promise.toIOLazy(() => s |> BT.StorageArea.get(key))
      |> IO.map(obj => Js.Dict.get(obj, key))
      |> IO.mapError(
           [%raw "err => 'localStorageGetError: ' + err.toString()"],
         );

    let set = v =>
      Relude.Js.Promise.toIOLazy(() =>
        s |> BT.StorageArea.set(Js.Dict.fromList([(key, v)]))
      )
      |> IO.mapError(
           [%raw "err => 'localStorageSetError: ' + err.toString()"],
         );
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