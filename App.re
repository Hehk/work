open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;

/* let simpleButton = { */
/*   let component = React.component("SimpleButton"); */

/*   (~children as _: list(React.syntheticElement), ()) => */
/*     component(hooks => { */
/*       let (count, setCount, hooks) = React.Hooks.state(0, hooks); */
/*       let increment = () => setCount(count + 1); */

/*       let wrapperStyle = */
/*         Style.[ */
/*           backgroundColor(Color.rgba(1., 1., 1., 0.1)), */
/*           border(~width=2, ~color=Colors.white), */
/*           margin(16), */
/*         ]; */

/*       let textHeaderStyle = */
/*         Style.[ */
/*           color(Colors.white), */
/*           fontFamily("Roboto-Regular.ttf"), */
/*           fontSize(20), */
/*         ]; */

/*       let textContent = "Click me: " ++ string_of_int(count); */
/*       ( */
/*         hooks, */
/*         <Clickable onClick=increment> */
/*           <View style=wrapperStyle> */
/*             <Padding padding=4> */
/*               <Text style=textHeaderStyle text=textContent /> */
/*             </Padding> */
/*           </View> */
/*         </Clickable>, */
/*       ); */
/*     }); */
/* }; */

let style = Style.[]
let textStyle = Style.[color(Colors.white), fontFamily("Hack-Regular.ttf"), fontSize(20)]

let button = {
  let component = React.component("Button");

  (~children as _: list(React.syntheticElement), ~onClick, ~text, ()) =>
    component(hooks => {
      let wrapperStyle =
        Style.[
          backgroundColor(Color.rgba(1., 1., 1., 0.1)),
          border(~width=2, ~color=Colors.white),
          margin(16),
        ];

      (hooks,
       <Clickable onClick>
        <View style=wrapperStyle>
          <Padding padding=4>
            <Text style=textStyle text />
          </Padding>
        </View>
       </Clickable>
      )
    })
}

type pr = {
  title: string,
  content: string,
  id: string,
};

type item =
  | PR(pr);

type actions =
  | MOVE_UP(string)
  | MOVE_DOWN(string);

let item = {
  let component = React.component("Item");

  (~children as _: list(React.syntheticElement), ~content, ()) =>
    component(hooks => {
      (
        hooks,
        switch (content) {
        | PR({title}) => <Text style=textStyle text=title />
        },
      )
    });
};

let itemList = {
  let component = React.component("ItemList");

  (
    ~children as _: list(React.syntheticElement),
    ~items,
    ~focus,
    ~dispatch,
    (),
  ) =>
    component(hooks => {
      (
        hooks,
        <View style>
          {items
           |> List.map(content => <item content />)
           |> React.listToElement}
        </View>,
      )
    });
};

let rec getIndex = (~f, ~index=0) =>
  fun
  | [] => None
  | [hd, ...tl] =>
    if (f(hd)) {
      Some(index);
    } else {
      getIndex(~f, ~index=index + 1, tl);
    };

let main = {
  let component = React.component("Main");

  (~children as _: list(React.syntheticElement), ()) =>
    component(hooks => {
      let (items, setItems, hooks) =
        React.Hooks.state(
          [
            /* PR({title: "hello", content: "", id: "1"}), */
            /* PR({title: "2", content: "2", id: "2"}), */
          ],
          hooks,
        );
      let (focus, setFocus, hooks) = React.Hooks.state("0", hooks);

      let dispatch = action =>
        switch (action) {
        | MOVE_UP(id) =>
          switch (
            getIndex(
              ~f=
                fun
                | PR(item) => item.id === id,
              items,
            )
          ) {
          | None => ()
          | Some(index) =>
            let newId =
              switch (List.nth(items, index === 0 ? index : index - 1)) {
              | PR(item) => item.id
              };
            setFocus(newId);
          }
        | MOVE_DOWN(id) =>
          switch (
            getIndex(
              ~f=
                fun
                | PR(item) => item.id === id,
              items,
            )
          ) {
          | None => ()
          | Some(index) =>
            let newId =
              switch (List.nth(items, index === List.length(items) - 1 ? index : index + 1)) {
              | PR(item) => item.id
              };
            setFocus(newId);
          }
        };

      let onClick = (_) => {
        let newItems = [PR({
          title: "Hello World",
            content: "Lorem ipsum...",
            id: items |> List.length |> string_of_int
        }), ...items];
        setItems(newItems);
      };

      (hooks, <View style>
       <itemList items focus dispatch />
       <button text="Create Item" onClick />
       </View>);
    });
};

let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Work");

  let element = <View style><main /></View>;

  let _ = UI.start(win, element);
  ();
};

App.start(init);
