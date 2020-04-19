let useClickHandler = (~delay=300, onClick, onDoubleClick) => {
  let singleClickDebouncer =
    Relude.Debounce.debounce(~delayMS=delay, onClick);
  React.useEffect0(() => Some(() => {singleClickDebouncer.cancel()}));

  let onClickHandler = _ => singleClickDebouncer.f();
  let onDoubleClickHandler = _ => {
    singleClickDebouncer.cancel();
    onDoubleClick();
  };
  (onClickHandler, onDoubleClickHandler);
};