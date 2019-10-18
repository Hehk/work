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

  let state = State.initialState();
  let win = App.createWindow(app, "Work");
  let element = <main state dispatch={State.update(~state)} />;

  let _ = Revery.Event.subscribe(win.onKeyDown, handleKeyDown);
  let update = UI.start(win, element);

  State.subscribe(newState => {
    update(<main state=newState dispatch={State.update(~state=newState)} />)
  });

  ();
};

App.start(init);
