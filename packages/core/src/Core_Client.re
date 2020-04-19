open Globals;
module Tree = Core_Tree;

module Action = {
  type t =
    | Activate(Tree.path)
    | Remove(Tree.path)
    | ToggleCollapse(Tree.path);
};

type t = {port: BPort.t(Tree.t, Action.t)};

let make = port => {port: port};
let send = (tree, {port, _}) => BPort.postMessage(tree, port);
let disconnect = ({port, _}) => BPort.disconnect(port);

let equal: (BPort.t('a, 'b), BPort.t('a, 'b)) => bool = [%raw
  "function (lhs, rhs) { return lhs === rhs }"
];
let equal = ({port: lhs, _}, {port: rhs, _}) => equal(lhs, rhs);

let pp = (fmt, t) => Format.fprintf(fmt, "(port=(name=%s))", t.port.name);
let show = t => Format.asprintf("%a", pp, t);

module Show: BsBastet.Interface.SHOW with type t = t = {
  type nonrec t = t;
  let show = show;
};
include Relude.Extensions.Show.ShowExtensions(Show);