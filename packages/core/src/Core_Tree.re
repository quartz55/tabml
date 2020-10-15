open Globals;
module Option = {
  include Option;
  let let_ = Option.bind;
};
module BAPI = Core_BAPI;
module Node = Core_Tree_Node;
module T = Relude.Tree;
module TZip = {
  include Relude.TreeZipper;
  let appendChild = (child, {children, _} as z) => {
    ...z,
    children: children @ [child],
  };
};

type path = list(Relude.TreeZipper.movement);
type node = Node.t;
type t = T.t(Node.t);

let placeholderWindow = winId =>
  BWindow.{
    id: Some(winId),
    alwaysOnTop: false,
    focused: false,
    height: None,
    incognito: false,
    left: None,
    sessionId: None,
    state: None,
    tabs: [||],
    title: None,
    top: None,
    type_: None,
    width: None,
  };

let rebuild = z => TZip.(moveUpToTop(z) |> getFocusTree);

let zipperAt = (path, t) =>
  TZip.fromTree(t) |> TZip.moveBy(path |> List.reverse);

let mapNodeAt = (fn, path, t) =>
  zipperAt(path, t) |> Option.map(TZip.modifyFocusValue(fn) >> rebuild);

let treeAt = (path, t) =>
  zipperAt(path, t) |> Option.map(TZip.getFocusTree);

let getNodeAt = (path, t) =>
  zipperAt(path, t) |> Option.map(TZip.getFocusValue);

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

let popNode = (~promoteChildren=false, z) => {
  let n =
    promoteChildren ? T.pure(TZip.getFocusValue(z)) : TZip.getFocusTree(z);
  deleteNode(~promoteChildren, z) |> Option.map(z => (n, z));
};

let appendWindowZipper = (w, t) =>
  TZip.fromTree(t)
  |> TZip.moveBy([`Down(1), `RightToEnd])
  |> Option.flatMap(TZip.insertWithPushLeft(w))
  |> Option.getOrElseLazy(() =>
       TZip.{
         ancestors: [([], T.getValue(t), [])],
         leftSiblings: [],
         focus: w,
         rightSiblings: [],
         children: [],
       }
     );

let findWindowWithId = (windowId, t) =>
  TZip.fromTree(t)
  |> TZip.findInFocusAndChildren(node =>
       switch (Node.type_(node)) {
       | Window({id: Some(id), _}) when Int.eq(id, windowId) => true
       | _ => false
       }
     );

let findWindowWithIdOrAppend = (windowId, t) =>
  findWindowWithId(windowId, t)
  |> Option.getOrElseLazy(() =>
       appendWindowZipper(placeholderWindow(windowId) |> Node.ofWindow, t)
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
//      aka BFS(Breadth First Search)
// let findWindowHeuristically = (windowId, t) => {};

let findTabWithId = (tabId, t) =>
  TZip.fromTree(t)
  |> TZip.findInFocusAndChildren(node =>
       switch (Node.type_(node)) {
       | Tab({id: Some(id), _}) when Int.eq(id, tabId) => true
       | _ => false
       }
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
  modifyTabWithId(const(newTab), tabId, t);

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

let moveTabWithIdToIdx = (~window=`Same, tabId, idx, t) => {
  let%Option tabZip = findTabWithId(tabId, t);
  let%Option winZip =
    deleteNode(~promoteChildren=true, tabZip)
    |> Option.flatMap(
         switch (window) {
         | `Same => findTabWindow
         | `OfId(winId) =>
           rebuild >> findWindowWithIdOrAppend(winId) >> Option.pure
         },
       );
  let tab = tabZip |> TZip.getFocusValue;
  let%Option (updatedWinZip, newParent) =
    winZip
    |> findNodeIndexed(~idxPred=Node.isTab, (_, i) => i == idx)
    |> Option.flatMap(((z, _)) => {
         let parent =
           TZip.moveUp(z)
           |> Option.map(TZip.getFocusValue >> Node.type_)
           |> Option.flatMap(
                fun
                | Node.Tab(t) => t.id
                | _ => None,
              );
         TZip.insertTreeWithPushRight(T.pure(tab), z)
         |> Option.map(z => (z, parent));
       })
    |> Option.orElseLazy(~fallback=() =>
         Some((TZip.appendChild(T.pure(tab), winZip), None))
       );
  Some((rebuild(updatedWinZip), newParent));
};

let addCreatedTab = (tab: BTab.t, t) => {
  let winId = tab.windowId;
  // TODO use findWindowHeuristically here
  // let%Option winZip = findWindowHeuristically(winId, t);
  let winZip = findWindowWithIdOrAppend(winId, t);
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
    |> Option.orElseLazy(~fallback=() => {
         Some(TZip.appendChild(tabNode, winZip))
       })
    |> Option.map(rebuild);
  };
};

// TODO this is going to need some cleanup
let bootstrap = () => {
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
    RJs.Promise.toIOLazy(() =>
      BTabs.(query(makeQueryObj(~windowId=Option.getOrThrow(w.id), ())))
    )
    |> IO.mapError(e => `BrowserError(e))
    |> IO.map(tabs => {
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

  BAPI.getAllWindows()
  |> IO.flatMap(windows =>
       Array.toList(windows)
       |> List.filter(w => w.BWindow.id != None)
       |> List.map(setupWindow)
       |> BAPI.IOE.all
     )
  |> IO.map(windowsAsTrees =>
       T.make(Node.make(Session("Current Session")), windowsAsTrees)
     );
};

let show = Relude.Tree.showPrettyBy(Node.Show.show);

let pp = fmt => show >> Format.fprintf(fmt, "%s");

module Path = {
  type t = path;

  let rec eq = (p1, p2) =>
    switch (p1, p2) {
    | ([`Down(n1), ...p1], [`Down(n2), ...p2])
    | ([`Right(n1), ...p1], [`Right(n2), ...p2]) when n1 == n2 =>
      eq(p1, p2)
    | ([], []) => true
    | _ => false
    };

  let (==) = eq;

  let show: t => string =
    List.reverse
    >> List.showBy(
         fun
         | `Down(x) => Format.sprintf("D%d", x)
         | `Right(x) => Format.sprintf("R%d", x)
         | _ => "_",
       );

  let pp = fmt => show >> Format.fprintf(fmt, "%s");
};

let showPath = Path.show;
