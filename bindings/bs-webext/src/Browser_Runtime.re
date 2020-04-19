open Browser_Types;

[@bs.val] [@bs.scope ("browser", "runtime")]
external getURL: string => string = "getURL";

[@bs.val] [@bs.scope ("browser", "runtime")]
external reload: unit => unit = "reload";

type browserInfo = {
  name: string,
  vendor: string,
  version: string,
  buildID: string,
};

[@bs.val] [@bs.scope ("browser", "runtime")]
external getBrowserInfo: unit => Js.Promise.t(browserInfo) = "getBrowserInfo";

[@bs.val] [@bs.scope ("browser", "runtime")]
external sendMessage: 'a => Js.Promise.t('b) = "sendMessage";

type conInfo;
[@bs.obj]
external makeConInfo:
  (~name: string=?, ~includeTlsChannelId: bool=?, unit) => conInfo;

[@bs.val] [@bs.scope ("browser", "runtime")]
external connect:
  (~extensionID: string=?, ~connectInfo: conInfo=?, unit) =>
  Port.t('send, 'recv) =
  "connect";

[@bs.val] [@bs.scope ("browser", "runtime")]
external onConnect: EventListener.t(Port.t('send, 'recv) => unit) =
  "onConnect";

[@bs.val] [@bs.scope ("browser", "runtime")]
external onMessage: EventListener.t(('msg, Js.t('a)) => unit) = "onMessage";