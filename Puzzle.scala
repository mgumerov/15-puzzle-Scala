object Puzzle {
  case class Board(cells: Array[Int]);

  case class Move(cellValue: Int);

  class Rules {
    final def initialBoard : Board = {
      Board((1 to 15).toArray ++ Array(0));
    }
    final def isValid(board: Board, move: Move) : Boolean = {
      val hole = board.cells.indexOf(0);
      move match {
        case x if (hole > 3 && move.cellValue == board.cells(hole-4)) => true;
        case x if (hole < 12 && move.cellValue == board.cells(hole+4)) => true;
        case x if (hole%4 != 0 && move.cellValue == board.cells(hole-1)) => true;
        case x if (hole%4 != 3 && move.cellValue == board.cells(hole+1)) => true;
        case _ => false;
      }
    }
    final def apply(board: Board, move: Move) : Option[Board] = {
      val active : Option[Int] = isValid(board, move) match {
        case false => None;
        case true => Some(move.cellValue);
      }
      active match {
        case None => None;
        case Some(target) => Some(Board(board.cells.map(x => x match {
                               case `target` => 0;
                               case 0 => target;
                               case _ => x;
                             })));
      }
    }
    final def randomMove(rndgen: scala.util.Random) : Move = { //not necessary valid move!
      Move(1 + rndgen.nextInt(15));
    }
  }

  private final def present(board: Board) {
    val b = board.cells.map(x => "%2d".format(x));
    println(b.slice(0,4).mkString(" "));
    println(b.slice(4,8).mkString(" "));
    println(b.slice(8,12).mkString(" "));
    println(b.slice(12,16).mkString(" "));
  }

  private final def shuffle(rules: Rules, rndgen: scala.util.Random, board: Board, moves: Int) : Board = {
    if (moves == 0) return board;
    rules.apply(board, rules.randomMove(rndgen)) match {
      case None => shuffle(rules, rndgen, board, moves); //move is not valid -> do not decrement remaining moves
      case Some(newboard) => shuffle(rules, rndgen, newboard, moves-1);
    }
  }

  @scala.annotation.tailrec
  private final def step(rules: Rules, board: Board) {
    present(board);

    val ln = scala.io.StdIn.readLine();
    if (ln == "") return;
    val move : Option[Move] = scala.util.Try(ln.toInt).toOption match {
      case Some(x) if 1 until 16 contains x => Some(Move(x));
      case _ => None;
    }

    val newboard : Option[Board] = move match {
      case None => Some(board);
      case Some(x) => rules.apply(board, x);
    };

    newboard match {
      case None => step(rules, board);
      case Some(x) => step(rules, x);
    };
  }

  def main(args: Array[String]) {
    val rules = new Rules;
    val shuffled = shuffle(rules, new scala.util.Random(), rules.initialBoard, 30);
    step(rules, shuffled);
  }
}
