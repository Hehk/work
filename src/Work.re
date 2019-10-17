open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;
open Item;

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

let style = Style.[];

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

  (~children as _: list(React.syntheticElement), ()) =>
    component(hooks => {
      let (items, setItems, hooks) = React.Hooks.state([], hooks);
      let (focus, setFocus, hooks) = React.Hooks.state("0", hooks);
      let hooks =
        React.Hooks.effect(
          OnMount,
          () => {
            let _ =
              Github.query(
                ~onLoad=
                  response => {
                    let items =
                      switch (response#viewer#repositories#nodes) {
                      | Some(nodes) =>
                        nodes
                        |> Array.to_list
                        |> flatMap(~f=item =>
                             PR({
                               title: item#name,
                               content: item#name,
                               id: item#id,
                             })
                           )
                      | None => []
                      };
                    setItems(items);
                  },
                GetPRs.make(),
              );
            None;
          },
          hooks,
        );

      /* let dispatch = action => */
      /*   switch (action) { */
      /*   | MOVE_UP(id) => */
      /*     switch ( */
      /*       getIndex( */
      /*         ~f= */
      /*           fun */
      /*           | PR(item) => item.id === id, */
      /*         items, */
      /*       ) */
      /*     ) { */
      /*     | None => () */
      /*     | Some(index) => */
      /*       let newId = */
      /*         switch (List.nth(items, index === 0 ? index : index - 1)) { */
      /*         | PR(item) => item.id */
      /*         }; */
      /*       setFocus(newId); */
      /*     } */
      /*   | MOVE_DOWN(id) => */
      /*     switch ( */
      /*       getIndex( */
      /*         ~f= */
      /*           fun */
      /*           | PR(item) => item.id === id, */
      /*         items, */
      /*       ) */
      /*     ) { */
      /*     | None => () */
      /*     | Some(index) => */
      /*       let newId = */
      /*         switch ( */
      /*           List.nth( */
      /*             items, */
      /*             index === List.length(items) - 1 ? index : index + 1, */
      /*           ) */
      /*         ) { */
      /*         | PR(item) => item.id */
      /*         }; */
      /*       setFocus(newId); */
      /*     } */
      /*   }; */

      (hooks, <View style> <itemList items focus /> </View>);
    });
};


