open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;

let style = Style.[];
let textStyle =
  Style.[
    color(Colors.white),
    fontFamily("Hack-Regular.ttf"),
    fontSize(20),
  ];

type pr = {
  title: string,
  content: string,
  id: string,
};

type item =
  | PR(pr);

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
    ~items: list(item),
    ~focus: option(string),
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
