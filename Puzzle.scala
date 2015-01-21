object Puzzle {
  private final def present(board: Array[Int]) {
    val b = board.map(x => "%2d".format(x));
    println(b.slice(0,4).mkString(" "));
    println(b.slice(4,8).mkString(" "));
    println(b.slice(8,12).mkString(" "));
    println(b.slice(12,16).mkString(" "));
  }

  @scala.annotation.tailrec
  private final def step(board: Array[Int]) {
    present(board);
    val ln = scala.io.StdIn.readLine();
    if (ln == "") return;
    val target = ln.toInt;
    val newboard = board.map(x => x match {
        case `target` => 0;
        case 0 => target;
        case _ => x;
      });
    step(newboard);
  }

  def main(args: Array[String]) {
    step((0 to 15).toArray);
  }
}
