object Puzzle {
  private final def present(board: Array[Int]) {
    val b = board.map(x => "%2d".format(x));
    println(b.slice(0,4).mkString(" "));
    println(b.slice(4,8).mkString(" "));
    println(b.slice(8,12).mkString(" "));
    println(b.slice(12,16).mkString(" "));
  }

  type Board = Array[Int];

  private final def shuffle(rndgen: scala.util.Random, board: Board, moves: Int) : Board = {
    if (moves == 0) return board;
    val move = 1 + rndgen.nextInt(15);
    apply(board, move) match {
      case None => shuffle(rndgen, board, moves); //move is not valid -> do not decrement remaining moves
      case Some(newboard) => shuffle(rndgen, newboard, moves-1);
    }
  }

  private final def apply(board: Board, move: Int) : Option[Board] = {
    val hole = board.indexOf(0);
    val active : Option[Int] = move match {
      case x if (hole > 3 && move == board(hole-4)) => Some(move);
      case x if (hole < 12 && move == board(hole+4)) => Some(move);
      case x if (hole%4 != 0 && move == board(hole-1)) => Some(move);
      case x if (hole%4 != 3 && move == board(hole+1)) => Some(move);
      case _ => None;
    }

    active match {
      case None => None;
      case Some(target) => Some(board.map(x => x match {
                             case `target` => 0;
                             case 0 => target;
                             case _ => x;
                           }));
    };
  }

  @scala.annotation.tailrec
  private final def step(board: Board) {
    present(board);

    val ln = scala.io.StdIn.readLine();
    if (ln == "") return;
    val move : Option[Int] = scala.util.Try(ln.toInt).toOption match {
      case Some(x) if 1 until 16 contains x => Some(x);
      case _ => None;
    }

    val newboard : Option[Board] = move match {
      case None => Some(board);
      case Some(x) => apply(board, x);
    };

    newboard match {
      case None => step(board);
      case Some(x) => step(x);
    };
  }

  def main(args: Array[String]) {
    val init : Board = (0 to 15).toArray;
    val shuffled = shuffle(new scala.util.Random(), init, 30);
    step(shuffled);
  }
}
