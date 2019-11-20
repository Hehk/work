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

let item = {
  let component = React.component("Item");

  (
    ~children as _: list(React.syntheticElement),
    ~content: State.pr,
    ~isFocus,
    ~focus: list(string),
    (),
  ) =>
    component(hooks => {
      (
        hooks,
        <Text
          style={isFocus ? focusTextStyle : textStyle}
          text={content.title}
        />,
      )
    });
};

let itemList = {
  let component = React.component("ItemList");

  (
    ~children as _: list(React.syntheticElement),
    ~items: list(State.node),
    ~focus: list(string),
    (),
  ) =>
    component(hooks => {
      (
        hooks,
        <View style>
          {items
           |> List.map((element: State.node) => {
                switch (element.content, focus) {
                | (PR(content), [hd, ...tl]) =>
                  <item content isFocus={hd === content.id} focus=tl />
                | (PR(content), []) =>
                  <item content isFocus=false focus=[] />
                }
              })
           |> React.listToElement}
        </View>,
      )
    });
};
