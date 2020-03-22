open Relude.Globals;
module Option = {
  include Option;
  let let_ = bind;
};
open Browser.Globals;

module Node = Core_Tree_Node;

module T = Relude.Tree;
module TZip = {
  include Relude.TreeZipper;
  let appendChild = (child, {children, _} as z) => {
    ...z,
    children: List.append(child, children),
  };
};

type node = Node.t;
type t = T.t(Node.t);

let rebuild = z => TZip.(moveUpToTop(z) |> getFocusTree);

let findNodeIndexed = (~idxPred=const(true), pred, z) => {
  let incIdx = (idx, z) => idxPred(TZip.getFocusValue(z)) ? idx + 1 : idx;
  let rec dft = (idx, z) =>
    pred(z |> TZip.getFocusValue, idx)
      ? `Found((z, idx))
      : TZip.moveDown(z)
        |> Option.map(z => dft(incIdx(idx, z), z))
        |> Option.map(
             fun
             | `NotFound(idx) =>
               TZip.moveRight(z)
               |> Option.map(z => dft(incIdx(idx, z), z))
               |> Option.getOrElse(`NotFound(idx))
             | found => found,
           )
        |> Option.orElseLazy(~fallback=() =>
             TZip.moveRight(z) |> Option.map(z => dft(incIdx(idx, z), z))
           )
        |> Option.getOrElse(`NotFound(idx));
  TZip.moveDown(z)
  |> Option.map(dft(0))
  |> Option.flatMap(
       fun
       | `NotFound(_) => None
       | `Found(z, idx) => Some((z, idx)),
     );
};

let findNode = (pred, t) =>
  TZip.fromTree(t) |> TZip.findInFocusAndChildren(n => pred(Node.type_(n)));

let mapNode = (fn, t) => {
  let memo = ref(None);
  findNode(fn >> Option.tap(m => memo := Some(m)) >> Option.isSome, t)
  |> Option.map(
       Node.update(_ => Option.getOrThrow(memo^)) |> TZip.modifyFocusValue,
     )
  |> Option.map(rebuild)
  |> Option.getOrElse(t);
};

let promoteNodeChildren = (TZip.{rightSiblings, children} as z) => {
  ...z,
  children: [],
  rightSiblings: children @ rightSiblings,
};

let deleteNode = (~promoteChildren=false) =>
  (promoteChildren ? promoteNodeChildren : id) >> TZip.delete;

let zipperOfTabIndex = (idx, windowZipper) => {
  if (!(windowZipper |> TZip.getFocusValue |> Node.isWindow)) {
    failwith("must be a zipper of a window");
  };
  let rec dft = (curr, z: TZip.t(Node.t)) =>
    curr < idx
      ? TZip.moveDown(z)
        |> Option.map(dft(curr + 1))
        |> Option.map(
             fun
             | `NotFound(idx) =>
               TZip.moveRight(z)
               |> Option.map(dft(idx + 1))
               |> Option.getOrElse(`NotFound(idx))
             | `Found(z) => `Found(z),
           )
        |> Option.orElseLazy(~fallback=() =>
             TZip.moveRight(z) |> Option.map(dft(curr + 1))
           )
        |> Option.getOrElse(`NotFound(curr))
      : `Found(z);
  dft(0, windowZipper |> TZip.moveDown |> Option.getOrThrow);
};

let findWindowWithId = (windowId, t) =>
  TZip.fromTree(t)
  |> TZip.findInFocusAndChildren(node =>
       switch (Node.type_(node)) {
       | Window({id: Some(id), _}) when Int.eq(id, windowId) => true
       | _ => false
       }
     )
  |> Option.tapNone(() =>
       failwith(Format.sprintf("Couldn't find window with id %d", windowId))
     );

let modifyWindowWithId = (fn, windowId, t) => {
  findWindowWithId(windowId, t)
  |> Option.map(TZip.modifyFocusValue(Node.updateWindow(fn)))
  |> Option.map(rebuild)
  |> Option.getOrElse(t);
};

let appendWindow = (w, t) => T.appendChild(~child=T.pure(w), t);

let removeWindowWithId = (windowId, t) =>
  findWindowWithId(windowId, t)
  |> Option.flatMap(TZip.delete)
  |> Option.map(rebuild)
  |> Option.getOrElse(t);

// TODO Try searching direct children of sessions first
let findWindowHeuristically = (windowId, t) => {};

let findTabWithId = (tabId, t) =>
  TZip.fromTree(t)
  |> TZip.findInFocusAndChildren(node =>
       switch (Node.type_(node)) {
       | Tab({id: Some(id), _}) when Int.eq(id, tabId) => true
       | _ => false
       }
     )
  |> Option.tapNone(() =>
       failwith(Format.sprintf("Couldn't find tab with id %d", tabId))
     );

let rec findTabWindow = tabZip =>
  switch (tabZip |> TZip.getFocusValue |> Node.type_) {
  | Tab(_) => TZip.moveUp(tabZip) |> Option.flatMap(findTabWindow)
  | Window(_) => Some(tabZip)
  | Session(_) => failwith("why is a tab a child of session????")
  };

let removeTabWithId = (tabId, t) =>
  findTabWithId(tabId, t)
  |> Option.flatMap(deleteNode(~promoteChildren=true))
  |> Option.map(rebuild)
  |> Option.getOrElse(t);

let modifyTabWithId = (fn, tabId, t) =>
  findTabWithId(tabId, t)
  |> Option.map(TZip.modifyFocusValue(Node.updateTab(fn)))
  |> Option.map(rebuild)
  |> Option.getOrElse(t);

let setTabWithId = (tabId, newTab, t) =>
  modifyTabWithId(_ => newTab, tabId, t);

let moveTabWithIdToIdx = (~window=`Same, tabId, idx, t) => {
  let%Option tabZip = findTabWithId(tabId, t);
  let%Option winZip =
    deleteNode(~promoteChildren=true, tabZip)
    |> Option.flatMap(
         switch (window) {
         | `Same => findTabWindow
         | `OfId(winId) => rebuild >> findWindowWithId(winId)
         },
       );
  let tab = tabZip |> TZip.getFocusValue;
  let%Option updatedWinZip =
    switch (zipperOfTabIndex(idx, winZip)) {
    | `NotFound(lastIdx) when lastIdx == idx - 1 =>
      winZip
      |> TZip.moveBy([`Down(1), `RightToEnd])
      |> Option.flatMap(TZip.insertTreeWithPushLeft(T.pure(tab)))
    | `Found(z) => TZip.insertTreeWithPushRight(T.pure(tab), z)
    | `NotFound(_) => failwith("unreachable?")
    };
  Some(rebuild(updatedWinZip));
};

let addCreatedTab = (tab: BTab.t, t) => {
  let winId = tab.windowId;
  // TODO
  // let%Option winZip = findWindowHeuristically(winId, t);
  let%Option winZip = findWindowWithId(winId, t);
  let tabNode = Node.ofTab(tab) |> T.pure;
  switch (tab.openerTabId) {
  | Some(opener) =>
    let%Option (openerZip, openerIdx) =
      winZip
      |> findNodeIndexed(~idxPred=Node.isTab, (n, _idx) =>
           switch (Node.type_(n)) {
           | Tab({id: Some(id), _}) when Int.eq(opener, id) => true
           | _ => false
           }
         );
    let relIdx = tab.index - openerIdx - 1;
    openerZip
    |> findNodeIndexed(~idxPred=Node.isTab, (_, idx) => idx == relIdx)
    |> Option.flatMap(((z, _)) => TZip.insertTreeWithPushRight(tabNode, z))
    |> Option.orElseLazy(~fallback=() =>
         Some(TZip.appendChild(tabNode, openerZip))
       )
    |> Option.map(rebuild);
  | None =>
    let idx = tab.index;
    winZip
    |> findNodeIndexed(~idxPred=Node.isTab, (_, i) => i == idx)
    |> Option.flatMap(((z, _)) => TZip.insertTreeWithPushRight(tabNode, z))
    |> Option.orElseLazy(~fallback=() =>
         Some(TZip.appendChild(tabNode, winZip))
       )
    |> Option.map(rebuild);
  };
};

let getAllTabs = () => B.Tabs.(query(makeQueryObj()));

let getAllWindows = () => B.Windows.getAll();

let bootstrap = () => {
  module IOE =
    IO.WithError({
      type t = Js.Promise.error;
    });

  let setupWindow = w => {
    let setupTabHierarchy = (parentId, tabs) => {
      let rec setupTabHierarchy' = (parentId, out, rest, nomatch) =>
        switch (rest) {
        | [] => (out |> List.reverse, nomatch |> List.reverse)
        | [{BTab.id: Some(id), openerTabId: Some(opener)} as t, ...xs]
            when opener == parentId =>
          let (children', nomatch') = setupTabHierarchy'(id, [], xs, []);
          let node = T.make(Node.make(Tab(t)), children');
          setupTabHierarchy'(parentId, [node, ...out], nomatch', nomatch);
        | [{BTab.id: None, openerTabId: Some(opener)} as t, ...xs]
            when opener == parentId =>
          let node = T.make(Node.make(Tab(t)), []);
          setupTabHierarchy'(parentId, [node, ...out], xs, nomatch);
        | [t, ...xs] =>
          setupTabHierarchy'(parentId, out, xs, [t, ...nomatch])
        };

      setupTabHierarchy'(parentId, [], tabs, []);
    };

    let rec setupWindowTree = (out, windowTabs: list(BTab.t)) => {
      switch (windowTabs) {
      | [] => out |> List.reverse
      | [{id: None} as t, ...rest] =>
        Js.log2("Tab without id", t);
        setupWindowTree([T.make(Node.make(Tab(t)), []), ...out], rest);
      | [{id: Some(parentId)} as t, ...rest] =>
        let (children, rest) = setupTabHierarchy(parentId, rest);
        setupWindowTree(
          [T.make(Node.make(Tab(t)), children), ...out],
          rest,
        );
      };
    };

    let node = Node.make(Window(w));
    B.Tabs.(query(makeQueryObj(~windowId=Option.getOrThrow(w.id), ())))
    |> IOE.map(tabs => {
         Js.log2("setting up window", w);
         Js.log2("tabs", tabs);
         tabs
         |> List.fromArray
         |> List.map(t => t.BTab.index)
         |> Array.fromList
         |> Js.log;
         tabs |> Array.toList |> setupWindowTree([]) |> T.make(node);
       });
  };

  getAllWindows()
  |> IOE.flatMap(windows =>
       Array.toList(windows)
       |> List.filter(w => w.BWindow.id != None)
       |> List.map(setupWindow)
       |> IOE.all
     )
  |> IOE.map(windowsAsTrees =>
       T.make(Node.make(Session("Current Session")), windowsAsTrees)
     );
};

let show = Relude.Tree.showPrettyBy(Node.Show.show);

let pp = fmt => show >> Format.fprintf(fmt, "%s");