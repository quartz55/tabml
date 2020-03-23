open Relude.Globals;
open Browser.Globals;

let createWinIO = opts =>
  Relude.Js.Promise.toIOLazy(() => Browser.Windows.create(opts));
let updateWinIO = (opts, win) =>
  Relude.Js.Promise.toIOLazy(() =>
    Browser.Windows.update(BWindow.id(win), opts)
  );

let mainAppWindow = ref(None);

let rec spawnAppWindow = () =>
  switch (mainAppWindow^) {
  | None =>
    createAppWindow()
    |> IO.tap(w => {
         //  let id = BWindow.id(w);
         //  let rm = () => mainAppWindow := None;
         //  let fn = [%raw
         //    "wId => { if (wId === w.id) { rm(); browser.windows.onRemoved.removeListener(fn); } }"
         //  ];
         //  BWindows.onRemoved |> EL.addListener(fn);
         mainAppWindow := Some(w)
       })
  | Some(w) =>
    updateWinIO(Browser.Windows.makeUpdateData(~focused=true, ()), w)
    |> IO.tap(w => mainAppWindow := Some(w))
  }
and createAppWindow = () =>
  createWinIO(
    Browser.Windows.makeCreateData(
      ~_type="popup",
      ~url="tabml.html",
      ~width=430,
      ~height=650,
      (),
    ),
  )
  |> IO.flatMap(
       updateWinIO(Browser.Windows.makeUpdateData(~top=1, ~left=0, ())),
     );

let run = () => {
  Browser.BrowserAction.onClicked
  |> EL.addListener((tab, data) => {
       Js.log3("clicked toolbar button", tab, data);
       spawnAppWindow()
       |> IO.unsafeRunAsync(w => {
            Js.log2("opened tabml window", w);
            ignore(w);
          });
     });
  Core.run();
};

// [@bs.val] [@bs.scope "document"]
// external addEventListener: (string, 'a => unit) => unit = "addEventListener";

// addEventListener("DOMContentLoaded", _ => run());

run();