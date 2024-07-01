package spaceship

import org.scalatest.funsuite.AnyFunSuiteLike

class SolverTest extends AnyFunSuiteLike:

  test("solve") {
    assert(Solver.solve(Seq(
      (0, -1), (1, -3), (3, -5), (6, -7), (9, -9), (13, -10)
    )) === Seq(2, 3, 6, 6, 5, 9))
  }