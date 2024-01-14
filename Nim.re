open Game;

module Nim: Game = {
  type whichPlayer =
    | P1
    | P2;

  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  type state =
    | State(status, int);

  type move =
    | M1
    | M2
    | M3;


  /* the state of the game when it begins */
  let initialState = _ => State(Ongoing(P1), 21);

  /* produce the set of legal moves at a state, represented as a list */
  let legalMoves: state => list(move) =
    inState =>
      switch (inState) {
      | State(_, 0) => []
      | State(_, 1) => [M1]
      | State(_, 2) => [M1, M2]
      | State(_, _) => [M1, M2, M3]
      };

  /* returns the status of the game at the given state */
  let gameStatus: state => status =
    inState => {
      let State(p, _) = inState;
      p;
    };

  let otherPlayer: whichPlayer => whichPlayer =
    player =>
      switch (player) {
      | P1 => P2
      | P2 => P1
      };




  /* given a state and a legal move, yields the next state */
  let nextState: (state, move) => state =
    (inState, inMove) =>
      switch (inState, inMove) {
      | (State(Win(_), _), _)
      | (State(Draw, _), _) => inState
      | (State(Ongoing(player), 1), M1) => State(Win(otherPlayer(player)), 0)
      | (State(Ongoing(player), 2), M2) => State(Win(otherPlayer(player)), 0)
      | (State(Ongoing(player), 3), M3) => State(Win(otherPlayer(player)), 0)
      | (State(Ongoing(player), x), M1) =>
        State(Ongoing(otherPlayer(player)), x - 1)
      | (State(Ongoing(player), x), M2) =>
        State(Ongoing(otherPlayer(player)), x - 2)
      | (State(Ongoing(player), x), M3) =>
        State(Ongoing(otherPlayer(player)), x - 3)
      };

  /* returns the current player of the game */
  let currentPlayer: state => whichPlayer =
    inState =>
      switch (inState) {
      | State(Draw, _) => failwith("")
      | State(Ongoing(p), _) => p
      | State(Win(p), _) => p
      };



  /* estimate the value of a given state */
  let estimateValue: state => float =
    inState =>
      switch (inState) {
      | State(Win(P1), _) => 1.
      | State(Win(P2), _) => -1.
      | State(_, x) => if (x mod 4 == 0) {1.} else {float_of_int(x mod 4)}
      };




// formula: 21, 17, 13, 9, 5, 1

  /* returns the string representation of the inputted player */
  let stringOfPlayer: whichPlayer => string =
    player =>
      if (player == P1) {
        "P1";
      } else {
        "P2";
      };

  /* returns the string representation of the inputted state*/
  let stringOfState: state => string =
    inState =>
      switch (inState) {
      | State(Win(p), _) => stringOfPlayer(p) ++ " wins!"
      | State(Ongoing(p), x) => "There are still " ++ string_of_int(x) ++ " matches left. " ++ stringOfPlayer(p) ++ "'s turn"
      | State(Draw, _) => "The game ends in a draw!"
      };

  /* returns the string representation of the inputted move */
  let stringOfMove: move => string =
    inMove =>
      switch (inMove) {
      | M1 => "takes 1"
      | M2 => "takes 2"
      | M3 => "takes 3"
      };
  /* return the move of the inputted string representation */
  let moveOfString: (string, state) => move =
    (str, sta) =>
      switch (str, sta) {
      | ("1", State(_, int)) =>
        if (int > 0) {
          M1;
        } else {
          failwith("There are not enough matches");
        }
      | ("2", State(_, int)) =>
        if (int > 1) {
          M2;
        } else {
          failwith("There are not enough matches");
        }
      | ("3", State(_, int)) =>
        if (int > 2) {
          M3;
        } else {
          failwith("There are not enough matches");
        }
      | _ => failwith("Not a legal Move")
      };
};

module Game: Game = Nim;




