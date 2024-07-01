package spaceship

object Solver:
  private def diff(pos: IndexedSeq[(Int, Int)]): IndexedSeq[(Int, Int)] =
    pos.indices.map { idx =>
      if idx == 0
      then pos(idx)
      else (pos(idx)._1 - pos(idx - 1)._1, pos(idx)._2 - pos(idx - 1)._2)
    }

  private val MAP = Map(
    (-1, -1) -> 1,
    (0, -1) -> 2,
    (1, -1) -> 3,
    (-1, 0) -> 4,
    (0, 0) -> 5,
    (1, 0) -> 6,
    (-1, 1) -> 7,
    (0, 1) -> 8,
    (1, 1) -> 9
  )

  def solve(pos: Seq[(Int, Int)]): Seq[Int] =
    diff(diff(pos.toIndexedSeq)).map(MAP)
