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

let item = {
  let component = React.component("Item");

  (~children as _: list(React.syntheticElement), ~content, ()) =>
    component(hooks => {
      (hooks, <Text style=textStyle text={content.title} />)
    });
};

let itemList = {
  let component = React.component("ItemList");

  (
    ~children as _: list(React.syntheticElement),
    ~items: list(pr),
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
