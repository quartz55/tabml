open Browser_Types;

type onClickData;
[@bs.val] [@bs.scope ("browser", "browserAction")]
external onClicked: EventListener.t((Tab.t, onClickData) => unit) =
  "onClicked";