type state = {
  focus: option(string),
  items: list(Item.pr),
};

type action =
  | AddPRs(list(Item.pr))
  | MoveUp
  | MoveDown;

let initialState = () => {
  {focus: None, items: []};
};

let subscribers: ref(list(state => unit)) = ref([]);
let state = ref(initialState());

let currentState = () => state^;
let dispatch = (action) => {
  let state = state^;
  let newState =
    switch (action) {
    | AddPRs(items) =>
      if (state.focus === None) {
        let first = List.hd(items);
        {focus: Some(first.id), items};
      } else {
        {...state, items};
      }
    | MoveUp => state
    | MoveDown => state
    };

  List.iter(f => f(newState), subscribers^);
};

let subscribe = cb => {
  subscribers := [cb, ...subscribers^];

  () => {
    subscribers := List.filter(f => f !== cb, subscribers^);
  };
};
