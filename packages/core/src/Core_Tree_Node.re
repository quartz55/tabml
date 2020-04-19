open Globals;

type t = {
  id: int,
  collapsed: bool,
  type_,
}
and type_ =
  | Session(string)
  | Window(BWindow.t)
  | Tab(BTab.t);

let _genId = {
  let curr = ref(0);
  () => {
    let out = curr^;
    curr := curr^ + 1;
    out;
  };
};

let make = (~collapsed=false, ~id=_genId(), type_) => {id, collapsed, type_};
// TODO is an id really needed/useful?
let id = ({id, _}) => id;
let type_ = ({type_, _}) => type_;
let collapsed = ({collapsed, _}) => collapsed;
let isCollapsed = collapsed;

let ofWindow = w => make(Window(w));
let ofTab = t => make(Tab(t));

let isWindow =
  fun
  | {type_: Window(_), _} => true
  | _ => false;
let isTab =
  fun
  | {type_: Tab(_), _} => true
  | _ => false;
let isSession =
  fun
  | {type_: Session(_), _} => true
  | _ => false;

let collapse = t => {...t, collapsed: true};
let expand = t => {...t, collapsed: false};
let toggleCollapsed = ({collapsed, _} as t) => {
  ...t,
  collapsed: !collapsed,
};

let update = (fn, {type_, _} as t) => {...t, type_: fn(type_)};
let updateTab = fn =>
  update(
    fun
    | Tab(t) => Tab(fn(t))
    | _ => failwith("not a tab"),
  );
let updateWindow = fn =>
  update(
    fun
    | Window(t) => Window(fn(t))
    | _ => failwith("not a window"),
  );
let updateSession = fn =>
  update(
    fun
    | Session(t) => Session(fn(t))
    | _ => failwith("not a session"),
  );

let show =
  fun
  | Session(name) => "[S] " ++ name
  | Window(w) =>
    "[W] "
    ++ (w.focused ? "(*) " : "")
    ++ Option.Infix.(Int.toString <$> w.BWindow.id |? "NO_ID")
    ++ " "
    ++ Option.Infix.(w.title |? "untitled")
  | Tab(t) =>
    "[T] "
    ++ (t.active ? "(*) " : "")
    ++ "("
    ++ Int.toString(t.index)
    ++ ") "
    ++ Option.Infix.((id => "[" ++ Int.toString(id) ++ "] ") <$> t.id |? "")
    ++ Option.Infix.(t.title |? "untitled");

let show = ({id: _id, type_}) => show(type_);

let pp = (fmt, t) => Format.fprintf(fmt, "%s", show(t));

module Show: BsBastet.Interface.SHOW with type t = t = {
  type nonrec t = t;
  let show = show;
};

include Relude.Extensions.Show.ShowExtensions(Show);

module Session = {
  let get =
    type_
    >> (
      fun
      | Session(name) => name
      | _ => failwith("not a session")
    );
};

module Window = {
  let get =
    type_
    >> (
      fun
      | Window(w) => w
      | _ => failwith("not a window")
    );
};

module Tab = {
  let get =
    type_
    >> (
      fun
      | Tab(t) => t
      | _ => failwith("not a tab")
    );
};