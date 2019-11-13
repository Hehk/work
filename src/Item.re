open Revery;
open Revery.Math;
open Revery.UI;
open Revery.UI.Components;

let style = Style.[];
let textStyle =
  Style.[
    color(Colors.gray),
    fontFamily("Hack-Regular.ttf"),
    padding(10),
    fontSize(20),
    paddingTop(5),
    paddingBottom(5),
    paddingRight(5),
    paddingLeft(5),
  ];
let focusTextStyle =
  Style.[
    color(Colors.black),
    backgroundColor(Colors.aliceBlue),
    fontFamily("Hack-Regular.ttf"),
    fontSize(20),
    paddingTop(5),
    paddingBottom(5),
    paddingRight(5),
    paddingLeft(5),
  ];

type prState = 
| Loaded
| Loading
| Error(string)

type pr = {
  id: string,
  title: string,
  state: prState
};


let item = {
  let component = React.component("Item");

  (~children as _: list(React.syntheticElement), ~content, ~focus, ()) =>
    component(hooks => {
      let isFocus =
        switch (focus) {
        | None => false
        | Some(id) => content.id === id
        };
      (
        hooks,
        <Text
          style={isFocus ? focusTextStyle : textStyle}
          text={content.title}
        />,
      );
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
           |> List.map(content => <item content focus />)
           |> React.listToElement}
        </View>,
      )
    });
};
