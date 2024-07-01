package spaceship

// WA
object Solver2:
  private def diff(pos: IndexedSeq[(Int, Int)]): IndexedSeq[(Int, Int)] =
    pos.indices.map { idx =>
      if idx == 0
      then pos(idx)
      else (pos(idx)._1 - pos(idx - 1)._1, pos(idx)._2 - pos(idx - 1)._2)
    }

  private def toCommands(v: (Int, Int)): Seq[Int] =
    Seq(
      Seq.fill(Math.max(-v._1, 0))(4),
      Seq.fill(Math.max(-v._2, 0))(2),
      Seq.fill(Math.max(v._1, 0))(6),
      Seq.fill(Math.max(v._2, 0))(8)
    ).flatten

  def solve(pos: Seq[(Int, Int)]): Seq[Int] =
    diff(diff(pos.toIndexedSeq)).flatMap(toCommands)
