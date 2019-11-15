open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;
open Item;
open State;

module GetPRs = [%graphql
  {|
query GetPRs {
  viewer {
    login

    repositories(first: 10, orderBy:{field:CREATED_AT, direction:DESC}){
      nodes{
        id
				name
      }
    }
  }
}
|}
];

let rootStyle =
  Style.[
    backgroundColor(Colors.white),
    color(Colors.black),
    position(`Absolute),
    top(0),
    left(0),
    right(0),
    bottom(0),
  ];

type actions =
  | MOVE_UP(string)
  | MOVE_DOWN(string);

let rec getIndex = (~f, ~index=0) =>
  fun
  | [] => None
  | [hd, ...tl] =>
    if (f(hd)) {
      Some(index);
    } else {
      getIndex(~f, ~index=index + 1, tl);
    };

let rec flatMap = (~f) =>
  fun
  | [hd, ...tl] =>
    switch (hd) {
    | Some(x) => [f(x), ...flatMap(~f, tl)]
    | None => flatMap(~f, tl)
    }
  | [] => [];

let main = {
  let component = React.component("Main");

  (
    ~children as _: list(React.syntheticElement),
    ~state: state,
    ~dispatch: action => unit,
    (),
  ) =>
    component(hooks => {
      let {items, focus} = state;
      let hooks =
        React.Hooks.effect(
          OnMount,
          () => {
            let _ =
              Github.query(
                ~onLoad=
                  response => {
                    switch (response#viewer#repositories#nodes) {
                    | Some(nodes) =>
                      nodes
                      |> Array.to_list
                      |> flatMap(~f=item =>
                           {title: item#name, id: item#id, state: Loading}
                         )
                      |> List.iter(item => dispatch(AddPR(item)))
                      |> ignore
                    | None => ()
                    }
                  },
                GetPRs.make(),
              );
            None;
          },
          hooks,
        );

      (hooks, <View style=rootStyle> <itemList items focus /> </View>);
    });
};
