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

let moveDown = (state) => {
  let rec getNextFocus = (focus, items : list(Item.pr)) => {
    switch items {
    | [] => focus
    | [hd] => focus
    | [hd, snd, ...tl] => 
      if (hd.id === focus) {
        snd.id
      } else {
        getNextFocus(focus, [snd, ...tl])
      };
    }
  }

  switch (state.focus) {
  | None => state
  | Some(focus) => 
    let newFocus = getNextFocus(focus, state.items);
    { ...state, focus: Some(newFocus) }
  }
};

let moveUp = (state) => {
  let rec getNextFocus = (focus, items : list(Item.pr)) => {
    switch items {
    | [] => focus
    | [hd] => focus
    | [hd, snd, ...tl] => 
      if (hd.id === focus) {
        snd.id
      } else {
        getNextFocus(focus, [snd, ...tl])
      };
    }
  }

  switch (state.focus) {
  | None => state
  | Some(focus) => 
    let newFocus = getNextFocus(focus, List.rev(state.items));
    { ...state, focus: Some(newFocus) }
  }
};


let currentState = () => state^;
let dispatch = action => {
  let oldState = state^;
  let newState =
    switch (action) {
    | AddPRs(items) =>
      if (oldState.focus === None) {
        let first = List.hd(items);
        {focus: Some(first.id), items};
      } else {
        {...oldState, items};
      }
    | MoveUp => moveUp(oldState)
    | MoveDown => moveDown(oldState)
    };

  List.iter(f => f(newState), subscribers^);
  state := newState
};

let subscribe = cb => {
  subscribers := [cb, ...subscribers^];

  () => {
    subscribers := List.filter(f => f !== cb, subscribers^);
  };
};
