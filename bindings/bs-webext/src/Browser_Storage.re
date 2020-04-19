open Browser_Types;

[@bs.val] [@bs.scope ("browser", "storage")]
external local: StorageArea.t = "local";

[@bs.val] [@bs.scope ("browser", "storage")]
external sync: StorageArea.t = "sync";

[@bs.val] [@bs.scope ("browser", "storage")]
external managed: StorageArea.t = "managed";