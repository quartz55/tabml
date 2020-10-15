open Globals;
open Core_Engine_Types;
module Store = Core_Store;
module Tree = Core_Tree;
module Client = Core_Client;
module BAPI = Core_BAPI;

type t = Core_Client.Action.t;

module Error = {
  type t = [ | `BrowserError(Js.Promise.error) | `ClientError(string)];
  let client: Logger.msgf('a) => t =
    msgf => `ClientError(msgf(fmt => Format.asprintf(fmt ^^ "")));

  let show =
    fun
    | `BrowserError(e) =>
      Format.sprintf("browser error: %s", Js.String.make(e))
    | `ClientError(e) => Format.sprintf("client error: %s", e);
};

module IOE = {
  include IO.WithError(Error);
  let let_ = bind;
};

let logPathError = (tree, path, ()) => {
  Logger.err(m =>
    m("Couldn't find node at %s\n%s", Tree.showPath(path), Tree.show(tree))
  );
  Store.NoUpdate;
};

let reducer = (state, action: t) => {
  let tree = state.session;

  switch (action) {
  | Activate(path) =>
    Logger.info(m => m("Activating node at %s", Tree.showPath(path)));
    Tree.getNodeAt(path, tree)
    |> Option.map(node =>
         Store.IO(
           _ =>
             switch (Tree.Node.type_(node)) {
             | Tab(t) =>
               BAPI.activateTab(~window=t.windowId, Option.getOrThrow(t.id))
               |> IO.map(ignore)
             | Window(w) =>
               BAPI.activateWindow(Option.getOrThrow(w.id))
               |> IO.map(ignore)
             | Session(_) => IO.pure()
             },
         )
       )
    |> Option.getOrElseLazy(logPathError(tree, path));

  | Remove(path) =>
    Logger.info(m => m("Removing node at %s", Tree.showPath(path)));
    Tree.getNodeAt(path, tree)
    |> Option.map(node =>
         Store.IO(
           _ =>
             switch (Tree.Node.type_(node)) {
             | Tab(t) => BAPI.removeTab(Option.getOrThrow(t.id))
             | Window(w) => BAPI.removeWindow(Option.getOrThrow(w.id))
             | Session(_) => IO.pure()
             },
         )
       )
    |> Option.getOrElseLazy(logPathError(tree, path));

  | ToggleCollapse(path) =>
    Logger.info(m =>
      m("Toggling collapse state of node at %s", Tree.showPath(path))
    );
    Tree.(mapNodeAt(Node.toggleCollapsed, path, tree))
    |> Option.map(tree => Store.Update({session: tree}))
    |> Option.getOrElseLazy(logPathError(tree, path));

  | Move(from, to_, false) =>
    Logger.info(m =>
      m(
        "Moving node %a to child of %a",
        Tree.Path.pp,
        from,
        Tree.Path.pp,
        to_,
      )
    );
    Tree.zipperAt(from, tree)
    |> Option.flatMap(z =>
         if (Tree.TZip.getFocusValue(z) |> Tree.Node.isCollapsed) {
           Logger.info(m => m("Node is collapsed, moving whole subree"));
           None;
         } else {
           Logger.info(m =>
             m("Node is expanded, promoting children and moving parent")
           );
           let node = Tree.TZip.getFocusValue(z);
           Tree.zipperAt(to_, tree)
           |> Option.map(({Tree.TZip.children} as z) =>
                {...z, children: [Tree.T.pure(node), ...children]}
              )
           |> Option.map(Tree.TZip.moveUpToTop)
           |> Option.flatMap(Tree.TZip.moveBy(from |> List.reverse))
           |> Option.flatMap(Tree.deleteNode(~promoteChildren=true))
           |> Option.map(z => Store.Update({session: Tree.rebuild(z)}));
           //  let destWinZip = dest |> Option.flatMap(Tree.findTabWindow);
         }
       )
    |> Option.getOrElse(Store.NoUpdate)
    |> ignore;
    Store.NoUpdate;

  | Move(from, to_, true) =>
    Logger.info(m =>
      m(
        "Moving node %a to next sibling of %a",
        Tree.Path.pp,
        from,
        Tree.Path.pp,
        to_,
      )
    );
    Store.NoUpdate;
  };
};
