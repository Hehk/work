type state = {
 focus: option(string),
 items: list(Item.item)
};

type action =
  | MoveUp(string)
  | MoveDown(string);

let initialState = () => {
 { focus: None, items: [] }
};

let subscribers : ref(list(state => unit)) = ref([]);
let update = (~state, action) => {

  let newState = switch (action) {
  | MoveUp(_) => state
  | MoveDown(_) => state
  };

  List.iter(f => f(newState), subscribers^);
};

let subscribe = (cb) => {
  subscribers := [cb, ...subscribers^];

  () => {
   subscribers := List.filter(f => f !== cb, subscribers^);
  }
};
