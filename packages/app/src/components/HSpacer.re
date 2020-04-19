let s = w =>
  Css.(
    style([
      display(inlineBlock),
      width(zero),
      height(zero),
      marginRight(w),
    ])
  );

[@react.component]
let make = (~width=Css.px(1)) => <div className={s(width)} />;