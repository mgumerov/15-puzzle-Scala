object Puzzle {
  @scala.annotation.tailrec
  private final def step(board: Array[Int]) {
    println(board.mkString(","));
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
