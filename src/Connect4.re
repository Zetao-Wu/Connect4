open! CS17SetupGame;
open Game;

module Connect4 = {
  /*
     P1 is Player1 and P2 is Player2.
   */
  type whichPlayer =
    | P1
    | P2;

  /*
     The status of a game is described as either a player has won,
     it's a draw, or it's still ongoing.
   */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /*
     The board will be represented as a list of list of ints where each
     list(int) represents a column and there is a list of these columns.

     The first int in each of the lists represents the most-bottom spot on the
     board and the columns of the lists go from left to right.
   */
  type board = list(list(int));

  /*
     The state of a game is represented by the status and the current board
     state.
   */
  type state =
    | State(status, board);

  /*
     A move will be made by an integer representing which column the piece will
     be dropped in to.
   */
  type move =
    | Move(int);

  /*
     Input: an integer, n, representing the length of each column in the
     Connect4 board.
     Output: a list of 0s of length n which represents the empty column.

     Recursion Statement: If the integer is 0, it outputs an empty list and if
     not it prints a list with 0 and recurs the procedure with the int - 1 until
     it reaches 0.
   */
  let rec createCol: int => list('a) =
    n =>
      switch (n) {
      | 0 => []
      | x => [0, ...createCol(x - 1)]
      };

  /*
     Input: an integer tuple which describes the height and width of the
     Connect4 board.
     Output: a list of list of 0s that represents the empty Connect4 board with
     the specified dimmensions.

     Recursion Statement: If the number of columns is 0, it print an empty list
     if not, it creates a list representing one column using createCol and
     the height and recurs over the same height and the width - 1.
   */
  let rec createBoard: (int, int) => list(list('a)) =
    (height, width) =>
      switch (height, width) {
      | (_, 0) => []
      | (h, w) => [createCol(h), ...createBoard(h, w - 1)]
      };

  /*
     Input: a string of two numbers which represents the height and width of
     the board.
     Output: the initial state which is ongoing, P1's turn, and the board is
     a list of list of 0s with a given height and width.
   */
  let initialState: string => state =
    s => {
      let boardDims = parseBoardDims(s);
      let boardHeight = getBoardHeight(boardDims);
      let boardWidth = getBoardWidth(boardDims);
      State(Ongoing(P1), createBoard(boardHeight, boardWidth));
    };

  /*
     Input: whichPlayer, which is either P1 or P2
     Output: either P1 or P2 as a string
   */
  let stringOfPlayer: whichPlayer => string =
    player =>
      if (player == P1) {
        "P1";
      } else {
        "P2";
      };

  /*
     Input: a list of list of ints, the board
     Output: the board where the order of the rows is reversed
   */

  let rowFlip: list(list(int)) => list(list(int)) =
    matrix => List.map(List.rev, matrix);

  /*
     Input: a list of list of ints, the board
     Output: the board where the order of the columns is reversed
   */

  let columnFlip: list(list(int)) => list(list(int)) =
    matrix => List.rev(matrix);

  /*
     Input: a list of list of ints, the board
     Output: a matrix that is transposed, or in other words, it is reflected
     across its main diagonal

     Recursion Diagrams:
       OI: [[1, 2, 3]]
       RI: [[2, 3]]
       RO: [[2], [3]]
       Ideation: cons the first of the list onto RO?
       make each element of the list separate?
       OO: [[1], [2], [3]]

       OI: [[1, 2, 3], [4, 5, 6]]
       RI: [[2, 3], [5, 6]]
       RO: [[2, 5], [3, 6]]
       Ideation: Take the first of the lists and make a new list and cons it onto
       the RO
       OO: [[1, 4], [2, 5], [3, 6]]
   */

  let rec transpose: list(list(int)) => list(list(int)) =
    matrix =>
      switch (matrix) {
      | []
      | [[], ..._] => []
      | [[_], ..._] => [List.flatten(matrix)]
      | [[_, ..._], ..._] => [
          List.map(List.hd, matrix),
          ...transpose(List.map(List.tl, matrix)),
        ]
      };

  /*
     Input: a state
     Output: a state with the board transposed and columnFlipped.
   */

  let transposeOfState: state => state =
    inState =>
      switch (inState) {
      | State(a, aloi) => State(a, columnFlip(transpose(aloi)))
      };

  /*
     Input: a state
     Output: a string representation of the board which separates each spot
     with | and represents an empty spot as 0, 1 for a spot filled by P1's
     piece, and 2 for a spot filled by P2's piece.

     Recursion Statement: If the board is empty, then output an empty string,
     if there is stuff in the board, then print a line, the first of the list,
     and recur through the first list until all of the elements are printed
     with a line between it. Then recur through the rest of the lists in the
     same manner.
   */

  let rec stringOfStateHelper: state => string =
    inState =>
      switch (inState) {
      | State(_, []) => ""
      | State(a, [[], ...tl2]) =>
        " | " ++ "\n" ++ stringOfStateHelper(State(a, tl2))
      | State(a, [[hd, ...tl], ...tl2]) =>
        " | "
        ++ string_of_int(hd)
        ++ stringOfStateHelper(State(a, [tl]))
        ++ stringOfStateHelper(State(a, tl2))
      };

  /*
     Input: a state
     Output: a string representation of the TRANSPOSED board which separates
     each spot with | and represents an empty spot as 0, 1 for a spot filled
     by P1's piece, and 2 for a spot filled by P2's piece.

     This transposed board represents the board in the context of Connect4
     accurately.
   */

  let stringOfState: state => string =
    inState => stringOfStateHelper(transposeOfState(inState));

  /*
     Input: a move
     Output: the string representation of the move, which is
     "puts a piece in column _"
   */
  let stringOfMove: move => string =
    inMove =>
      switch (inMove) {
      | Move(x) => "puts a piece in column " ++ string_of_int(x)
      };

  /*
     Input: a list of ints representing a single column
     Output: a boolean which represents whether another piece can be put into
     that column, in other words, if there is a 0 at the end of the column list.

     Recursion Statement: It outputs false if the list is empty. If there is one
     element in the list and it equals 0, it outputs true, if it doesn't equal 0,
     it outputs false. If there is more than one element in the list, it recurs
     through the list so that there is only one element in the list, the last
     element.
   */
  let rec legalMoveHelper2: list(int) => bool =
    column =>
      switch (column) {
      | [] => false
      | [hd] =>
        if (hd == 0) {
          true;
        } else {
          false;
        }
      | [_, ...tl] => legalMoveHelper2(tl)
      };

  /*
     Input: a list of list of ints which represents the entire board and the int
     which will always start at 1 to check all of the moves for legality.
     Output: the list of possible moves which represents which columns are
     available.

     Recursion Statement: If the list is empty, then there are no legal moves
     so the result is an empty list as well. Given a board that is not an empty
     list, it checks if the last element in that column list is 0 with
     legalMoveHelper2. If it is, it creates a list with Move(n) and recurs on
     the tail of the board and n + 1, and if not, it jsut recurs on the tail of
     the baord and n + 1.
   */
  let rec legalMoveHelper: (list(list(int)), int) => list(move) =
    (lst, n) =>
      switch (lst, n) {
      | ([], _) => []
      | ([hd, ...tl], n) =>
        if (legalMoveHelper2(hd) == true) {
          [Move(n), ...legalMoveHelper(tl, n + 1)];
        } else {
          legalMoveHelper(tl, n + 1);
        }
      };

  /*
     Input: a state
     Output: the list of legal moves by using the two helpers above.
   */
  let legalMoves: state => list(move) =
    inState =>
      switch (inState) {
      | State(_, lst) => legalMoveHelper(lst, 1)
      };

  /*
     Input: a state
     Output: the status at the state
   */
  let gameStatus: state => status =
    inState => {
      let State(p, _) = inState;
      p;
    };

  /*
     Input: a player, either P1 or P2
     Output: the other player, if the input is P1 then P2, and vice versa
   */
  let otherPlayer: whichPlayer => whichPlayer =
    player =>
      switch (player) {
      | P1 => P2
      | P2 => P1
      };

  /*
     Input: a list of list of int and move tuple which represents the board and
     the move.
     Output: the column in which the move is taking place

     Recursion Statement: The procedure fails if the list is empty. If there are
     columns in the list, then now it checks the integer inside move. If the int
     is 1, then it prints the first column of the board, if not, it keeps
     recurring on the tail of the board and Move(n-1) until the integer in move
     reaches 1.
   */

  let rec nextMoveColumn: (list(list(int)), move) => list(int) =
    (board, move) =>
      switch (board, move) {
      | ([], _) => failwith("The board has no dimensions.")
      | ([hd, ..._], Move(1)) => hd
      | ([_, ...tl], Move(n)) => nextMoveColumn(tl, Move(n - 1))
      };

  /*
     Input: a list of list of int and move tuple which represents the board and
     the move.
     Output: a list of the columns that follow the column found in
     nextMoveColumn

     Recursion Statement: The procedure fails if the list is empty. If there are
     columns in the list, then now it checks the integer inside move. If the int
     is 1, then it prints the rest of the board, if not, it keeps recurring on
     the tail of the board and Move(n-1) until the integer in move reaches 1.
   */

  let rec afterColumn: (list(list(int)), move) => list(list(int)) =
    (board, move) =>
      switch (board, move) {
      | ([], _) => failwith("The board has no dimensions.")
      | ([_, ...tl], Move(1)) => tl
      | ([_, ...tl], Move(n)) => afterColumn(tl, Move(n - 1))
      };

  /*
     Input: a list of list of int and move tuple which represents the board and
     the move.
     Output: a list of the columns that come before the column found in
     nextMoveColumn

     Recursion Statement: The procedure fails if the list is empty. If there are
     columns in the list, then now it checks the integer inside move. If the int
     is 1, then it prints an empty list since there would be nothing before the
     first column. If the int is 2, then it print the list with just the first
     column. For any other number, it prints the first column and recurs on
     the tail of the board and Move(n-1) until the int in move reaches 1.
   */

  let rec beforeColumn: (list(list(int)), move) => list(list(int)) =
    (board, move) =>
      switch (board, move) {
      | ([], _) => failwith("a")
      | ([_, ..._], Move(1)) => []
      | ([hd, ..._], Move(2)) => [hd]
      | ([hd, ...tl], Move(n)) => [hd, ...beforeColumn(tl, Move(n - 1))]
      };

  /*
     Input: a list of int and move tuple where the list represents a column and
     the move represents any move.
     Output: the column list with the first 0 in the list replaced by a 1.

     Recursion Statement: If the column list is empty, then it outputs a
     failwith. If the column list is not empty, it will check if the first
     element in the column is 0, in which case it will output the same column
     with the first element changed to a 1. If the first element is not 0, it
     will print a list with the first element and the recurrence of the rest
     of the list and the move until it finds a 0 in the column. If not, it will
     eventually become an empty list and output the failwith.
   */

  let rec nextMoveP1: (list(int), move) => list(int) =
    (column, move) =>
      switch (column) {
      | [] => failwith("The column is already full, not a legal move.")
      | [hd, ...tl] =>
        if (hd == 0) {
          [1, ...tl];
        } else {
          [hd, ...nextMoveP1(tl, move)];
        }
      };

  /*
     Input: a list of int and move tuple where the list represents a column and
     the move represents any move.
     Output: the column list with the first 0 in the list replaced by a 2.

     Recursion Statement: If the column list is empty, then it outputs a
     failwith. If the column list is not empty, it will check if the first
     element in the column is 0, in which case it will output the same column
     with the first element changed to a 2. If the first element is not 0, it
     will print a list with the first element and the recurrence of the rest
     of the list and the move until it finds a 0 in the column. If not, it will
     eventually become an empty list and output the failwith.
   */

  let rec nextMoveP2: (list(int), move) => list(int) =
    (column, move) =>
      switch (column) {
      | [] => failwith("a")
      | [hd, ...tl] =>
        if (hd == 0) {
          [2, ...tl];
        } else {
          [hd, ...nextMoveP2(tl, move)];
        }
      };

  /*
     Input: a state and move tuple
     Output: the resulting board after the move has been made
   */

  let combinedNextMove: (state, move) => list(list(int)) =
    (state, move) =>
      switch (state, move) {
      | (State(Win(_), _), _) => failwith("The game is already over.")
      | (State(Draw, _), _) => failwith("The game is already over.")
      | (State(Ongoing(player), board), move) =>
        let changedColumnP1 = nextMoveP1(nextMoveColumn(board, move), move);
        let changedColumnP2 = nextMoveP2(nextMoveColumn(board, move), move);
        let beforeColumn = beforeColumn(board, move);
        let afterColumn = afterColumn(board, move);
        if (player == P1) {
          List.append(
            List.append(beforeColumn, [changedColumnP1]),
            afterColumn,
          );
        } else {
          List.append(
            List.append(beforeColumn, [changedColumnP2]),
            afterColumn,
          );
        };
      };

  /*
     Input: a list of list of ints which represents the board
     Output: the main diagonal of the board

     Recursion Statement: If the list is empty, then output an empty list. If
     the board has columns, then conduct another switch on the first column and
     output a list with the first element of the column and the recurrence of
     mapping List.tl on the tail of the board which is a board without
     the first row and column.
   */

  let rec findDiagonal: list(list(int)) => list(int) =
    board =>
      switch (board) {
      | [] => []
      | [hd, ...tl] =>
        switch (hd) {
        | [] => []
        | [hd, ..._] => [hd, ...findDiagonal(List.map(List.tl, tl))]
        }
      };

  /*
     Input: a list of list of ints which represents the board
     Output: a list of the diagonals starting from the first value of each
     column.

     Recursion Statement: If the list is empty, output an empty list. If the
     list has elements then set restDiagonals equal to the recursive output
     of the tail of the board. Then construct a list with the first element
     being the result of running findDiagonal on the board and then
     restDiagonals. This creates a list such that findDiagonal is run on every
     board from the original to the recursive ones.
   */

  let rec findDiagonal2: list(list(int)) => list(list(int)) =
    board =>
      switch (board) {
      | [] => []
      | [_, ...tl] =>
        let restDiagonals = findDiagonal2(tl);
        [findDiagonal(board), ...restDiagonals];
      };

  /*
     Input: a list of list of ints that represents the board
     Output: a list of the diagonals that go from left to right that start from
     both the first value of each column and all of the values of the
     first column.

     Note: The column list goes from bottom to top. The columns go from left to
     right.
   */

  let allDiagonals1: list(list(int)) => list(list(int)) =
    board =>
      List.append(
        findDiagonal2(board),
        findDiagonal2(rowFlip(transpose(columnFlip(board)))),
      );

  /*
     Input: a list of list of ints that represents the board
     Output: a list of the diagonals that go from right to left that start from
     both the first value of each column and all of the values of the
     first column.

     Note: The column list goes from bottom to top. The columsn go fro left to
     right.
   */

  let allDiagonals2: list(list(int)) => list(list(int)) =
    board =>
      List.append(
        findDiagonal2(columnFlip(board)),
        findDiagonal2(rowFlip(transpose(board))),
      );

  /*
     Input: a list of list of ints that represents the board
     Output: all of the diagonals, the result of appending the outputs of
     allDiagonals1 and allDiagonals2 on the board.
   */

  let allDiagonal: list(list(int)) => list(list(int)) =
    board => List.append(allDiagonals1(board), allDiagonals2(board));

  /*
     Input: a list of list of ints which represents the list of all diagonals
     Output: a list of list of ints which represents the list of the diagonals
     without any duplicates.

     Recursion Statement: The function checks if the first diagonal is anywhere
     in the tail of the list of diagonals. If so, it recurs through
     removeDuplicate by calling the tail, if not, it creates a list including
     the first diagonal and also the recursion through the tail of the
     diagonals.
   */

  let rec removeDuplicate: list(list(int)) => list(list(int)) =
    diagonals =>
      switch (diagonals) {
      | [] => []
      | [hd, ...tl] =>
        if (List.mem(hd, tl)) {
          removeDuplicate(tl);
        } else {
          [hd, ...removeDuplicate(tl)];
        }
      };

  /*
     Input: a list of list of ints which represents the board
     Output: a list of list of ints which represents the final list of all of
     the diagonals without duplicates.
   */

  let allDiagonalFinal: list(list(int)) => list(list(int)) =
    board => removeDuplicate(allDiagonal(board));

  /*
     Input: a list of int that represents a column
     Output: a boolean which outputs true if there is a four in a row of either
     1s or 2s, and false otherwise.

     Recursion Statement: If the list is empty, it outputs false. If the first
     four elements are either all 1s or all 2s, it outputs true. If not, it
     recurs through the tail until it either finds a four in a row to output
     true or until the list becomes empty and outputs false.
   */

  let rec check: list(int) => bool =
    column =>
      switch (column) {
      | [] => false
      | [1, 1, 1, 1, ..._] => true
      | [2, 2, 2, 2, ..._] => true
      | [_, ...tl] => check(tl)
      };

  /*
     Input: a list of list of int that represents a board
     Output: a list of bools that is the result of maping the check function
     on every column of the board.
   */

  let columnCheck: list(list(int)) => list(bool) =
    board => List.map(check, board);

  /*
     Input: a list of list of int that represents a board
     Output: a list of bools that is the result of applying columnCheck on the
     transpose of the board which ultimately checks if there are four in a rows
     in all of the rows.
   */

  let rowCheck: list(list(int)) => list(bool) =
    board => columnCheck(transpose(board));

  /*
     Input: a list of list of int that represents a board
     Output: a list of bools that is the result of mapping the check function
     on every diagonal of the board.
   */

  let diagonalCheck: list(list(int)) => list(bool) =
    board => List.map(check, allDiagonalFinal(board));

  /*
     Input: a list of list of int that represents a board
     Output: a list of bools that is the result of appending the results of
     columnCheck, rowCheck, and diagonalCheck on the board.
   */

  let combinedCheck: list(list(int)) => list(bool) =
    board =>
      List.append(
        List.append(columnCheck(board), rowCheck(board)),
        diagonalCheck(board),
      );

  /*
     Input: a list of anything
     Output: a boolean denoting whether the list is empty or not
   */

  let isEmpty: list('a) => bool =
    alod =>
      switch (alod) {
      | [] => true
      | [_, ..._] => false
      };

  /*
     Input: any state
     Output: If the state is a Win or a Draw, it will output the same state. If
     the state is Ongoing, then it will check if there is a true element in the
     result of running combinedCheck on the board. If there is, it will output
     a win for the player. If not, it will next check if legalMoves of the state
     is empty, in which case it will output a draw. If both of these are false,
     then it will result as Ongoing for the other player.
   */

  let winCheck: state => state =
    state =>
      switch (state) {
      | State(Win(a), b) => State(Win(a), b)
      | State(Draw, a) => State(Draw, a)
      | State(Ongoing(player), board) =>
        let checkResult = combinedCheck(board);
        if (List.mem(true, checkResult)) {
          State(Win(player), board);
        } else if (isEmpty(legalMoves(state))) {
          State(Draw, board);
        } else {
          State(Ongoing(otherPlayer(player)), board);
        };
      };

  /*
     Input: a state and move tuple
     Output: If the status is a Win or a Draw, it will output a failwith. If the
     status is Ongoing, then output the state that is the result of running
     winCheck on the state with the board portion undergoing combinedNextMove on
     the entire (state, move) tuple.
   */

  let nextState: (state, move) => state =
    (s, m) =>
      switch (s, m) {
      | (State(Win(_), _), _) => failwith("The game is already over.")
      | (State(Draw, _), _) => failwith("The game is already over.")
      | (State(Ongoing(player), aloi), move) =>
        winCheck(
          State(
            Ongoing(player),
            combinedNextMove(State(Ongoing(player), aloi), move),
          ),
        )
      };

  /*
     Input: a list of moves
     Output: a list of ints that represents the int that is enclosed in a move

     Recursion Statement: If the list of moves is empty, then output an empty
     list. If not, then output the integer, n, inside Move(n) and recurse through
     the tail of the list until it becomes an empty list.
   */
  let rec moveOfStringHelper: list(move) => list(int) =
    lst =>
      switch (lst) {
      | [] => []
      | [Move(x), ...tl] => [x, ...moveOfStringHelper(tl)]
      };

  /*
     Input: a string state tuple where the string is a string representation of
     an integer which represents the move, and the state is a state
     Output: a move. If the string representation of move is a member of
     legalMoves after undergoing moveOfStringHelper, then the move is ouputted.
     If it is not a member of legalMoves, then it outputs a failwith.
   */

  let moveOfString: (string, state) => move =
    (str, inState) =>
      switch (str, inState) {
      | (x, State(_, _)) =>
        if (List.mem(
              int_of_string(x),
              moveOfStringHelper(legalMoves(inState)),
            )) {
          Move(int_of_string(x));
        } else {
          failwith("This column is full, please pick another column.");
        }
      };

  /* ESTIMATE VALUE */

  /*
     Input: a list of int that represents a column
     Output: If the list is empty, it outputs 0. If the list has three 1s in a
     row, it outputs 1, if the list has three 2s in a row, it outputs -1.

     Recursion Statement: It will recur through the tail of the column until it
     finds any of the three outcomes listed above.
   */
  let rec threeInARowCheck: list(int) => int =
    column =>
      switch (column) {
      | [] => 0
      | [1, 1, 1, ..._] => 1
      | [2, 2, 2, ..._] => (-1)
      | [_, ...tl] => threeInARowCheck(tl)
      };

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of mapping threeInARowCheck
     on the board.
   */

  let columnCheck3: list(list(int)) => list(int) =
    board => List.map(threeInARowCheck, board);

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of conducting columnCheck3 on
     the transpose of the board, which ultimately deals with the rows.
   */

  let rowCheck3: list(list(int)) => list(int) =
    board => columnCheck3(transpose(board));

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of mapping threeInARowCheck
     on all of the diagonals of the board.
   */

  let diagonalCheck3: list(list(int)) => list(int) =
    board => List.map(threeInARowCheck, allDiagonalFinal(board));

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of appending the result of
     columnCheck3, rowCheck3, and diagonalCheck3 on the board.
   */

  let combinedCheck3: list(list(int)) => list(int) =
    board =>
      List.append(
        List.append(columnCheck3(board), rowCheck3(board)),
        diagonalCheck3(board),
      );

  /*
     Input: a list of int that is the result of combinedCheck3 which is a list
     where there is a 1 for every triple for P1 and a 2 for every triple for P2,
     and a 0 everywhere else.
     Output: a float that is the result of adding 0.2 for every 1 and subtracting
     0.2 for every 2 that is in the list of ints.

     Recursion Statement: The empty list outputs 0. and it will keep going
     through the list adding 0.2 and -0.2 until it hits the empty list.
   */

  let rec tripleScore: list(int) => float =
    score =>
      switch (score) {
      | [] => 0.
      | [1, ...tl] => 0.2 +. tripleScore(tl)
      | [(-1), ...tl] => (-0.2) +. tripleScore(tl)
      | [_, ...tl] => tripleScore(tl)
      };

  /*
     Input: a list of int that represents a column
     Output: If the list is empty, it outputs 0. If the list has two 1s in a
     row, it outputs 1, if the list has two 2s in a row, it outputs -1.

     Recursion Statement: It will recur through the tail of the column until it
     finds any of the three outcomes listed above.
   */

  let rec twoInARowCheck: list(int) => int =
    column =>
      switch (column) {
      | [] => 0
      | [1, 1, ..._] => 1
      | [2, 2, ..._] => (-1)
      | [_, ...tl] => twoInARowCheck(tl)
      };

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of mapping twoInARowCheck
     on the board.
   */

  let columnCheck2: list(list(int)) => list(int) =
    board => List.map(twoInARowCheck, board);

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of conducting columnCheck2
     on the transpose of the board.
   */

  let rowCheck2: list(list(int)) => list(int) =
    board => columnCheck2(transpose(board));

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of mapping twoInARowCheck
     on all of the diagonals of the board.
   */

  let diagonalCheck2: list(list(int)) => list(int) =
    board => List.map(twoInARowCheck, allDiagonalFinal(board));

  /*
     Input: a list of list of int that represents a board
     Output: a list of integers that is the result of appending the result of
     columnCheck2, rowCheck2, and diagonalCheck2 on the board.
   */

  let combinedCheck2: list(list(int)) => list(int) =
    board =>
      List.append(
        List.append(columnCheck2(board), rowCheck2(board)),
        diagonalCheck2(board),
      );

  /*
     Input: a list of int that is the result of combinedCheck2 which is a list
     where there is a 1 for every double for P1 and a 2 for every double for P2,
     and a 0 everywhere else.
     Output: a float that is the result of adding 0.05 for every 1 and
     subtracting 0.05 for every 2 that is in the list of ints.

     Recursion Statement: The empty list outputs 0. and it will keep going
     through the list adding 0.05 and -0.05 until it hits the empty list.
   */

  let rec doubleScore: list(int) => float =
    score =>
      switch (score) {
      | [] => 0.
      | [1, ...tl] => 0.005 +. doubleScore(tl)
      | [(-1), ...tl] => (-0.005) +. doubleScore(tl)
      | [_, ...tl] => doubleScore(tl)
      };

  /*
     Input: a list of list of int that represents a board
     Output: a float of adding the results of doubleScore on combinedCheck2 of
     the board and tripleScore on combinedCheck3 of the board.
   */

  let bothScore: list(list(int)) => float =
    board =>
      doubleScore(combinedCheck2(board))
      +. tripleScore(combinedCheck3(board));

  /*
     Input: any state
     Output: a float representing how well P1 or P2 is doing with 1. representing
     a win for P1, -1. represeting a win for P2, and 0. representing a draw. For
     any values in between, whichever value it is closer to, 1. or -1. the better
     that player is doing.
   */

  let estimateValue: state => float =
    inState =>
      switch (inState) {
      | State(Win(P1), _) => 1.
      | State(Win(P2), _) => (-1.)
      | State(Draw, _) => 0.
      | State(Ongoing(_), board) => bothScore(board)
      };
};


open Connect4;

/* Check-expects */

// createCol
checkExpect(createCol(0), [], "create a column of 0s of length 0");
checkExpect(
  createCol(4),
  [0, 0, 0, 0],
  "create a column of 0s of length 4",
);

// createBoard
checkExpect(createBoard(5, 0), [], "create a 5 x 0 board of 0s");
checkExpect(
  createBoard(5, 7),
  [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
  ],
  "create a 5 x 7 board of 0s",
);

// initialState
checkExpect(
  initialState("5 7"),
  State(
    Ongoing(P1),
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ),
  "initial state of a 5 x 7 board",
);

// stringOfPlayer
checkExpect(stringOfPlayer(P1), "P1", "string of P1");
checkExpect(stringOfPlayer(P2), "P2", "string of P2");

// rowFlip
checkExpect(
  rowFlip([[1, 2, 3]]),
  [[3, 2, 1]],
  "rowFlip of a 1 x 3 board with random unofficial values",
);
checkExpect(
  rowFlip([[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
  [[3, 2, 1], [6, 5, 4], [9, 8, 7]],
  "rowFlip of a 3 x 3 board with random unofficial values",
);

// columnFlip
checkExpect(
  columnFlip([[1, 2, 3]]),
  [[1, 2, 3]],
  "columnFlip of a 1 x 3 board with random unofficial values",
);
checkExpect(
  columnFlip([[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
  [[7, 8, 9], [4, 5, 6], [1, 2, 3]],
  "columnFlip of a 3 x 3 board with random unofficial values",
);

// transpose
checkExpect(
  transpose([[1, 2, 3]]),
  [[1], [2], [3]],
  "transpose of a 1 x 3 board with random unofficial values",
);
checkExpect(
  transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]]),
  [[1, 4, 7], [2, 5, 8], [3, 6, 9]],
  "transpose of a 3 x 3 board with random unofficial values",
);

// transposeOfState
checkExpect(
  transposeOfState(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    ),
  ),
  State(
    Ongoing(P2),
    [[0, 0, 0, 0], [1, 0, 2, 1], [1, 0, 2, 2], [1, 0, 2, 1]],
  ),
  "transposeOfState of a random state",
);

// stringOfStateHelper
checkExpect(
  stringOfStateHelper(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    ),
  ),
  " | 1 | 1 | 1 | 0 | \n | 0 | 0 | 0 | 0 | \n | 2 | 2 | 2 | 0 | \n | 1 | 2 | 1 | 0 | \n",
  "stringOfStateHelper of a random state",
);

// stringOfState
checkExpect(
  stringOfState(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    ),
  ),
  " | 0 | 0 | 0 | 0 | \n | 1 | 0 | 2 | 1 | \n | 1 | 0 | 2 | 2 | \n | 1 | 0 | 2 | 1 | \n",
  "stringOfState of a random state",
);

// stringOfMove
checkExpect(
  stringOfMove(Move(1)),
  "puts a piece in column 1",
  "stringOfMove of Move(1)",
);
checkExpect(
  stringOfMove(Move(3)),
  "puts a piece in column 3",
  "stringOfMove of Move(3)",
);

// legalMoveHelper2
checkExpect(
  legalMoveHelper2([]),
  false,
  "legalMoveHelper2 of an empty list",
);
checkExpect(
  legalMoveHelper2([1, 2, 1, 2]),
  false,
  "legalMoveHelper2 of a full column list",
);
checkExpect(
  legalMoveHelper2([1, 2, 1, 0]),
  true,
  "legalMoveHelper2 of a open column list",
);

// legalMoveHelper
checkExpect(legalMoveHelper([], 1), [], "legalMoveHelper of an empty list");
checkExpect(
  legalMoveHelper(
    [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 2, 1, 2]],
    1,
  ),
  [],
  "legalMoveHelper of a draw board",
);
checkExpect(
  legalMoveHelper(
    [[1, 1, 1, 2], [0, 0, 0, 0], [2, 2, 2, 1], [1, 2, 1, 0]],
    1,
  ),
  [Move(2), Move(4)],
  "legalMoveHelper of a random board",
);
checkExpect(
  legalMoveHelper(
    [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 0, 0]],
    1,
  ),
  [Move(1), Move(2), Move(3), Move(4)],
  "legalMoveHelper of a board with all moves being legal",
);

// legalMoves
checkExpect(
  legalMoves(
    State(
      Draw,
      [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 2, 1, 2]],
    ),
  ),
  [],
  "legalMoves of a draw state",
);
checkExpect(
  legalMoves(
    State(
      Ongoing(P1),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 0, 0]],
    ),
  ),
  [Move(1), Move(2), Move(3), Move(4)],
  "legalMoves of a random state",
);

// gameStatus
checkExpect(
  gameStatus(
    State(
      Win(P1),
      [[1, 1, 1, 1], [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0]],
    ),
  ),
  Win(P1),
  "gameStatus of a win state",
);
checkExpect(
  gameStatus(
    State(
      Draw,
      [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 2, 1, 2]],
    ),
  ),
  Draw,
  "gameStatus of a draw state",
);
checkExpect(
  gameStatus(
    State(
      Ongoing(P1),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 0, 0]],
    ),
  ),
  Ongoing(P1),
  "gameStatus of an ongoing state",
);

// otherPlayer
checkExpect(otherPlayer(P1), P2, "otherPlayer of P1");
checkExpect(otherPlayer(P2), P1, "otherPlayer of P2");

// nextMoveColumn
checkExpect(
  nextMoveColumn(
    [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    Move(2),
  ),
  [0, 0, 0, 0],
  "nextMoveColumn of a random board and move",
);
checkExpect(
  nextMoveColumn(
    [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
    Move(3),
  ),
  [2, 2, 1, 0],
  "nextMoveColumn of a random board and move",
);

// afterColumn
checkExpect(
  afterColumn(
    [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    Move(1),
  ),
  [[0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
  "afterColumn of a random board and Move(1)",
);
checkExpect(
  afterColumn(
    [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
    Move(3),
  ),
  [[1, 2, 1, 1]],
  "afterColumn of a random board and move",
);

// beforeColumn
checkExpect(
  beforeColumn(
    [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    Move(1),
  ),
  [],
  "beforeColumn of a random board and Move(1)",
);
checkExpect(
  beforeColumn(
    [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    Move(2),
  ),
  [[1, 1, 1, 0]],
  "beforeColumn of a random board and Move(2)",
);
checkExpect(
  beforeColumn(
    [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
    Move(3),
  ),
  [[1, 1, 1, 0], [1, 1, 0, 0]],
  "beforeColumn of a random board and move",
);

// nextMoveP1
checkExpect(
  nextMoveP1([1, 1, 1, 0], Move(1)),
  [1, 1, 1, 1],
  "nextMoveP1 of a random column and move",
);
checkExpect(
  nextMoveP1([2, 0, 0, 0], Move(2)),
  [2, 1, 0, 0],
  "nextMoveP1 of a random column and move",
);

// nextMoveP2
checkExpect(
  nextMoveP2([1, 1, 1, 0], Move(1)),
  [1, 1, 1, 2],
  "nextMoveP2 of a random column and move",
);
checkExpect(
  nextMoveP2([2, 0, 0, 0], Move(2)),
  [2, 2, 0, 0],
  "nextMoveP2 of a random column and move",
);

// combinedNextMove
checkExpect(
  combinedNextMove(
    State(
      Ongoing(P1),
      [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
    ),
    Move(1),
  ),
  [[1, 1, 1, 1], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
  "combinedNextMove of a random state and move",
);
checkExpect(
  combinedNextMove(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
    ),
    Move(2),
  ),
  [[1, 1, 1, 0], [1, 1, 2, 0], [2, 2, 1, 0], [1, 2, 1, 1]],
  "combinedNextMove of a random state and move",
);

// findDiagonal
checkExpect(findDiagonal([]), [], "findDiagonal of an empty list");
checkExpect(
  findDiagonal([[1, 1, 1, 0], [1, 2, 0, 0], [2, 2, 2, 0], [1, 2, 1, 1]]),
  [1, 2, 2, 1],
  "the main diagonal of a random board",
);

// findDiagonal2
checkExpect(findDiagonal2([]), [], "findDiagonal of an empty list");
checkExpect(
  findDiagonal2([[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]]),
  [[1, 1, 1, 1], [1, 2, 1], [2, 2], [1]],
  "findDiagonal2 of a random board",
);

// allDiagonals1
checkExpect(allDiagonals1([]), [], "allDiagonals1 of an empty list");
checkExpect(
  allDiagonals1([[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]]),
  [
    [1, 1, 1, 1],
    [1, 2, 1],
    [2, 2],
    [1],
    [1, 1, 1, 1],
    [1, 0, 0],
    [1, 0],
    [0],
  ],
  "allDiagonals1 of a random board",
);

// allDiagonals2
checkExpect(allDiagonals2([]), [], "allDiagonals2 of an empty list");
checkExpect(
  allDiagonals2([[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]]),
  [
    [1, 2, 0, 0],
    [2, 1, 1],
    [1, 1],
    [1],
    [1, 2, 0, 0],
    [2, 1, 0],
    [1, 0],
    [1],
  ],
  "allDiagonals2 of a random board",
);

// allDiagonal
checkExpect(allDiagonal([[]]), [[], []], "allDiagonal of an empty list");
checkExpect(
  allDiagonal([[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]]),
  [
    [1, 1, 1, 1],
    [1, 2, 1],
    [2, 2],
    [1],
    [1, 1, 1, 1],
    [1, 0, 0],
    [1, 0],
    [0],
    [1, 2, 0, 0],
    [2, 1, 1],
    [1, 1],
    [1],
    [1, 2, 0, 0],
    [2, 1, 0],
    [1, 0],
    [1],
  ],
  "allDiagonal of a random board",
);

// removeDuplicate
checkExpect(removeDuplicate([]), [], "removeDuplicate of an empty list");
checkExpect(
  removeDuplicate(
    allDiagonal([[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 1]]),
  ),
  [
    [1, 2, 1],
    [2, 2],
    [1, 1, 1, 1],
    [1, 0, 0],
    [0],
    [2, 1, 1],
    [1, 1],
    [1, 2, 0, 0],
    [2, 1, 0],
    [1, 0],
    [1],
  ],
  "removeDuplicate of a random board",
);

// allDiagonalFinal
checkExpect(allDiagonalFinal([]), [], "allDiagonalFinal of an empty list");
checkExpect(
  allDiagonalFinal([
    [1, 1, 1, 0],
    [1, 1, 0, 0],
    [2, 2, 1, 0],
    [1, 2, 1, 1],
  ]),
  [
    [1, 2, 1],
    [2, 2],
    [1, 1, 1, 1],
    [1, 0, 0],
    [0],
    [2, 1, 1],
    [1, 1],
    [1, 2, 0, 0],
    [2, 1, 0],
    [1, 0],
    [1],
  ],
  "allDiagonalFinal of a random board",
);

// check
checkExpect(check([]), false, "check of an empty column");
checkExpect(
  check([2, 1, 2, 1, 1, 1, 1, 0]),
  true,
  "check on a column with 4 1s in a row",
);
checkExpect(
  check([2, 1, 2, 2, 2, 2, 0, 0]),
  true,
  "check on a column with 4 2s in a row",
);

// columnCheck
checkExpect(columnCheck([]), [], "columnCheck on an empty list");
checkExpect(
  columnCheck([[1, 1, 1, 2], [1, 1, 1, 1], [2, 2, 1, 0], [1, 2, 1, 0]]),
  [false, true, false, false],
  "columnCheck on a board with a win",
);

// rowCheck
checkExpect(rowCheck([]), [], "rowCheck on an empty list");
checkExpect(
  rowCheck([[1, 1, 1, 2], [1, 1, 1, 1], [2, 2, 1, 0], [1, 2, 1, 0]]),
  [false, false, true, false],
  "columnCheck on a board with a win",
);

// diagonalCheck
checkExpect(diagonalCheck([]), [], "diagonalCheck on an empty list");
checkExpect(
  diagonalCheck([[1, 1, 1, 2], [1, 1, 2, 1], [2, 2, 1, 0], [1, 2, 1, 1]]),
  [false, false, true, false, false, false, false, false, false, false],
  "diagonalCheck of a board with a diagonal win",
);

// combinedCheck
checkExpect(combinedCheck([]), [], "combinedCheck of an empty list");
checkExpect(
  combinedCheck([[1, 1, 1, 2], [1, 1, 2, 1], [2, 2, 1, 0], [1, 2, 1, 1]]),
  [
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
  ],
  "combinedCheck of a random board with a win",
);

// isEmpty
checkExpect(isEmpty([]), true, "isEmpty of an empty list");
checkExpect(isEmpty([1, 1, 1, 0]), false, "isEmpty of a non-empty list");

// winCheck
checkExpect(
  winCheck(
    State(
      Win(P1),
      [[1, 1, 1, 2], [1, 1, 2, 1], [2, 2, 1, 0], [1, 2, 1, 1]],
    ),
  ),
  State(
    Win(P1),
    [[1, 1, 1, 2], [1, 1, 2, 1], [2, 2, 1, 0], [1, 2, 1, 1]],
  ),
  "winCheck of a win state",
);
checkExpect(
  winCheck(
    State(
      Draw,
      [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 1, 1, 2]],
    ),
  ),
  State(Draw, [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 1, 1, 2]]),
  "winCheck of a draw state",
);
checkExpect(
  winCheck(
    State(
      Ongoing(P1),
      [[1, 1, 1, 2], [1, 1, 2, 1], [2, 2, 1, 0], [1, 2, 1, 1]],
    ),
  ),
  State(
    Win(P1),
    [[1, 1, 1, 2], [1, 1, 2, 1], [2, 2, 1, 0], [1, 2, 1, 1]],
  ),
  "winCheck of a supposesdly ongoing state but in fact a win state",
);
checkExpect(
  winCheck(
    State(
      Ongoing(P1),
      [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 1, 1, 2]],
    ),
  ),
  State(Draw, [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 1, 1, 2]]),
  "winCheck of a supposedly ongoing state but in fact a draw state",
);
checkExpect(
  winCheck(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 0]],
    ),
  ),
  State(
    Ongoing(P1),
    [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 0]],
  ),
  "winCheck on ongoing board",
);

// nextState
checkExpect(
  nextState(
    State(
      Ongoing(P1),
      [[1, 2, 1, 2], [1, 1, 2, 2], [2, 2, 1, 0], [1, 2, 1, 0]],
    ),
    Move(4),
  ),
  State(
    Win(P1),
    [[1, 2, 1, 2], [1, 1, 2, 2], [2, 2, 1, 0], [1, 2, 1, 1]],
  ),
  "nextState of a state and move that ends in a win",
);
checkExpect(
  nextState(initialState("5 7"), Move(4)),
  State(
    Ongoing(P2),
    [
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0],
    ],
  ),
  "nextState of the initialState and a move",
);

// moveOfStringHelper
checkExpect(moveOfStringHelper([]), [], "moveOfStringHelper on empty list");
checkExpect(
  moveOfStringHelper([Move(1), Move(2), Move(3)]),
  [1, 2, 3],
  "moveOfStringHelper on a list of moves",
);

// moveOfString
checkExpect(
  moveOfString(
    "1",
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 0]],
    ),
  ),
  Move(1),
  "moveOfString of valid move and state",
);

// bothScore is a culmination of all of the small very simple helpers in between
checkExpect(
  bothScore([[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]]),
  -0.015,
  "bothScore of a random board",
);
checkExpect(
  bothScore([[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 0]]),
  0.43000000000000005,
  "bothScore of a random board",
);

// estimateValue
checkExpect(
  estimateValue(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [0, 0, 0, 0], [2, 2, 2, 0], [1, 2, 1, 0]],
    ),
  ),
  -0.015,
  "estimateValue of a random board",
);
checkExpect(
  estimateValue(
    State(
      Ongoing(P2),
      [[1, 1, 1, 0], [1, 1, 0, 0], [2, 2, 1, 0], [1, 2, 1, 0]],
    ),
  ),
  0.43000000000000005,
  "estimateValue of a random board",
);

/* Check Errors */
checkError(
  () => nextMoveP1([1, 2, 1, 2], Move(1)),
  "The column is already full, not a legal move.",
);

checkError(
  () =>
    combinedNextMove(
      State(
        Win(P1),
        [[1, 1, 1, 1], [2, 0, 0, 0], [2, 0, 0, 0], [2, 0, 0, 0]],
      ),
      Move(1),
    ),
  "The game is already over.",
);
checkError(
  () =>
    combinedNextMove(
      State(
        Draw,
        [[1, 1, 1, 2], [2, 2, 2, 1], [2, 2, 2, 1], [1, 1, 1, 2]],
      ),
      Move(1),
    ),
  "The game is already over.",
);
checkError(
  () =>
    moveOfString(
      "1",
      State(
        Ongoing(P2),
        [[1, 1, 1, 2], [2, 2, 2, 0], [2, 2, 2, 1], [1, 1, 1, 0]],
      ),
    ),
  "This column is full, please pick another column.",
);