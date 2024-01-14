open! CS17SetupGame;
open Game;

module AIPlayer = (Connect4: Game) => {
  module PlayerGame = Connect4;
  /* TODO */

  let rec minimax: (PlayerGame.state, int) => float =
    (state, depth) =>
      switch (depth) {
      | 0 => PlayerGame.estimateValue(state)
      | _d =>
        let rec maxy: (list(float), float, int) => float = (
          (alon, currentMax, n) =>
            if (n == 0) {
              currentMax;
            } else if (List.hd(alon) > currentMax) {
              maxy(List.tl(alon), List.hd(alon), n - 1);
            } else {
              maxy(List.tl(alon), currentMax, n - 1);
            }
        );
        let rec mini: (list(float), float, int) => float = (
          (alon, currentMin, n) =>
            if (n == 0) {
              currentMin;
            } else if (List.hd(alon) < currentMin) {
              mini(List.tl(alon), List.hd(alon), n - 1);
            } else {
              mini(List.tl(alon), currentMin, n - 1);
            }
        );
        switch (PlayerGame.gameStatus(state)) {
        | Win(P1) => infinity
        | Win(P2) => infinity *. (-1.)
        | Draw => 0.0
        | Ongoing(P1) =>
          let z =
            List.map(
              x => minimax(x, depth - 1),
              List.map(
                y => PlayerGame.nextState(state, y),
                PlayerGame.legalMoves(state),
              ),
            );
          maxy(z, List.hd(z), List.length(z));
        | Ongoing(P2) =>
          let z =
            List.map(
              x => minimax(x, depth - 1),
              List.map(
                y => PlayerGame.nextState(state, y),
                PlayerGame.legalMoves(state),
              ),
            );
          mini(z, List.hd(z), List.length(z));
        };
      };

  let rec getMove:
    (
      list(PlayerGame.move),
      list(float),
      int,
      PlayerGame.move,
      float,
      string
    ) =>
    PlayerGame.move =
    (x, y, count, currentMove, currentValue, player) =>
      if (count == 0) {
        currentMove;
      } else {
        let (newMove, newValue) =
          if (player == "P1") {
            if (List.hd(y) > currentValue) {
              (List.hd(x), List.hd(y));
            } else {
              (currentMove, currentValue);
            };
          } else if (List.hd(y) < currentValue) {
            (List.hd(x), List.hd(y));
          } else {
            (currentMove, currentValue);
          };
        getMove(
          List.tl(x),
          List.tl(y),
          count - 1,
          newMove,
          newValue,
          player,
        );
      };

  let nextMove: PlayerGame.state => PlayerGame.move =
    s =>
      switch (PlayerGame.legalMoves(s)) {
      | [] => failwith("no available moves")
      | [hd, ...tl] =>
        let evaluationScores =
          List.map(
            move => minimax(PlayerGame.nextState(s, move), 3),
            [hd, ...tl],
          );
        switch (PlayerGame.gameStatus(s)) {
        | Ongoing(P1) =>
          getMove(
            [hd, ...tl],
            evaluationScores,
            List.length([hd, ...tl]),
            List.hd([hd, ...tl]),
            List.hd(evaluationScores),
            "P1",
          )
        | Ongoing(P2) =>
          getMove(
            [hd, ...tl],
            evaluationScores,
            List.length([hd, ...tl]),
            List.hd([hd, ...tl]),
            List.hd(evaluationScores),
            "P2",
          )
        | _ => failwith("no available moves")
        };
      };
  /* put your team name here! */
  let playerName = "marlon";
};

module TestGame = Connect4.Connect4;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer;
open TestAIPlayer;

/* Test-case */
checkExpect(
  minimax(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 0]],
    ),
    3,
  ),
  infinity,
  "Minimax",
);

checkExpect(
  minimax(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    ),
    3,
  ),
  infinity *. (-1.),
  "Minimax",
);
