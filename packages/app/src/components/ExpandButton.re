type state = [ | `Collapsed | `Expanded | `Disabled];

let plusSign = "+";
let minusSign = "-";
let dotSign = {js|\u22c5|js};

let style = canClick =>
  Css.(
    style([
      color(white),
      border(px(1), solid, white),
      cursor(canClick ? pointer : auto),
      textAlign(center),
      display(flexBox),
    ])
  );

[@react.component]
let make = (~state: state, ~onClick: unit => unit) => {
  let sign =
    switch (state) {
    | `Disabled => dotSign
    | `Expanded => minusSign
    | `Collapsed => plusSign
    };
  let canClick = state != `Disabled;
  let onClick =
    canClick
      ? e => {
          ReactEvent.Mouse.preventDefault(e);
          ReactEvent.Mouse.stopPropagation(e);
          onClick();
        }
      : Relude.Function.const();

  <div className={style(canClick)} onClick>
    <span> {React.string(sign)} </span>
  </div>;
};