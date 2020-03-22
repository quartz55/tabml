module I = {
  module Types = Browser_Types;
  module Tabs = Browser_Tabs;
  module Windows = Browser_Windows;
  module Runtime = Browser_Runtime;
  module Storage = Browser_Storage;
  module BrowserAction = Browser_BrowserAction;
};
include I;

module Globals = {
  module B = I;
  module BT = Browser_Types;

  module BPort = Browser_Types.Port;
  module EL = Browser_Types.EventListener;

  module BWindow = Browser_Types.Window;
  module BWindows = Browser_Windows;

  module BTab = Browser_Types.Tab;
  module BTabs = Browser_Tabs;
};