open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;
open Work;

let handleKeyDown = ({key}: Revery.Events.keyEvent) => {
  print_endline("Keyboard: " ++ Revery.Key.toString(key));
};

let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Work");
  let element = <View> <main /> </View>;

  let _ = Revery.Event.subscribe(win.onKeyDown, handleKeyDown);
  let _ = UI.start(win, element);
  ();
};

App.start(init);
