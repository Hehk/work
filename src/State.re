type prState =
  | Loaded
  | Loading
  | Error(string);

type pr = {
  id: string,
  title: string,
  state: prState,
};

type element =
  | PR(pr);

type node = {
  id: string,
  content: element,
  children: list(node),
};

type state = {
  focus: list(string),
  items: list(node),
};

type action =
  | AddPR(pr)
  | MoveUp
  | MoveDown
  | MoveIn
  | Esc;

let initialState = () => {
  {focus: [], items: []};
};

let subscribers: ref(list(state => unit)) = ref([]);
let state = ref(initialState());

module Actions = {
  let moveDown = state => {
    let rec getNextFocus = (focus, items: list(node)) => {
      switch (items) {
      | [] => focus
      | [hd] => focus
      | [hd, snd, ...tl] =>
        if (hd.id === focus) {
          snd.id;
        } else {
          getNextFocus(focus, [snd, ...tl]);
        }
      };
    };

    switch (state.focus) {
    | [] => state
    | [focus, ..._] =>
      let newFocus = getNextFocus(focus, state.items);
      {...state, focus: [newFocus]};
    };
  };

  let moveUp = state => {
    let rec getNextFocus = (focus, items: list(node)) => {
      switch (items) {
      | [] => focus
      | [hd] => focus
      | [hd, snd, ...tl] =>
        if (hd.id === focus) {
          snd.id;
        } else {
          getNextFocus(focus, [snd, ...tl]);
        }
      };
    };

    switch (state.focus) {
    | [] => state
    | [focus, ..._] =>
      let newFocus = getNextFocus(focus, List.rev(state.items));
      {...state, focus: [newFocus]};
    };
  };

  // TODO: implement addPR
  let addPr = state => {
    print_endline("ADD_PR not implemented");
    state;
  };
  // TODO: implement MoveIn
  let moveIn = state => {
    print_endline("MOVE_IN not implemented");
    state;
  };
  // TODO: implement Esc
  let escape = state => {
    print_endline("ESCAPE not implemented");
    state;
  };
};

let currentState = () => state^;
let dispatch = action => {
  let oldState = state^;
  let newState =
    switch (action) {
    | AddPR(items) => Actions.addPr(oldState)
    | MoveUp => Actions.moveUp(oldState)
    | MoveDown => Actions.moveDown(oldState)
    | MoveIn => Actions.moveIn(oldState)
    | Esc => Actions.escape(oldState)
    };

  List.iter(f => f(newState), subscribers^);
  state := newState;
};

let subscribe = cb => {
  subscribers := [cb, ...subscribers^];

  () => {
    subscribers := List.filter(f => f !== cb, subscribers^);
  };
};
