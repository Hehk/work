open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;
open State;
open Root;

let handleKeyDown = ({key}: Revery.Events.keyEvent) => {
  switch (key) {
  | KEY_J => dispatch(MoveDown)
  | KEY_K => dispatch(MoveUp)
  | KEY_ENTER => dispatch(MoveIn)
  | KEY_ESCAPE => dispatch(Esc)
  | _ => ()
  };
  print_endline("Keyboard: " ++ Revery.Key.toString(key));
};

let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Work");
  let element = <main state={currentState()} dispatch />;

  let _ = Revery.Event.subscribe(win.onKeyDown, handleKeyDown);
  let update = UI.start(win, element);

  State.subscribe(newState => {update(<main state=newState dispatch />)})
  |> ignore;

  ();
};

App.start(init);
