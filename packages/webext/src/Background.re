open Globals;

let createWinIO = opts => RJs.Promise.toIOLazy(() => BWindows.create(opts));
let updateWinIO = (opts, win: BWindow.t) =>
  RJs.Promise.toIOLazy(() =>
    BWindows.update(Option.getOrThrow(win.id), opts)
  );

let createAppWindow = () =>
  createWinIO(
    BWindows.makeCreateData(
      ~_type=`detached_panel,
      ~url="tabml.html",
      ~width=430,
      ~height=650,
      (),
    ),
  )
  |> IO.flatMap(updateWinIO(BWindows.makeUpdateData(~top=1, ~left=0, ())));

let run = () => {
  B.BrowserAction.onClicked
  |> EL.addListener((tab, data) => {
       Js.log3("clicked toolbar button", tab, data);
       createAppWindow()
       |> IO.unsafeRunAsync(w => {
            Js.log2("opened tabml window", w);
            ignore(w);
          });
     });
  Core.run();
};

run();
