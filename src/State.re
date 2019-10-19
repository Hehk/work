type state = {
  focus: option(string),
  items: list(Item.pr),
};

type action =
  | AddPRs(list(Item.pr))
  | MoveUp(string)
  | MoveDown(string);

let initialState = () => {
  {focus: None, items: []};
};

let subscribers: ref(list(state => unit)) = ref([]);
let update = (~state, action) => {
  let newState =
    switch (action) {
    | AddPRs(items) =>
      if (state.focus === None) {
        let first = List.hd(items);
        {focus: Some(first.id), items};
      } else {
        {...state, items};
      }
    | MoveUp(_) => state
    | MoveDown(_) => state
    };

  List.iter(f => f(newState), subscribers^);
};

let subscribe = cb => {
  subscribers := [cb, ...subscribers^];

  () => {
    subscribers := List.filter(f => f !== cb, subscribers^);
  };
};
