package basic

import org.scalatest.funsuite.AnyFunSuiteLike

class ExpTest extends AnyFunSuiteLike:

  test("apply") {
    assert(
      Exp("B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK".split(" ").toSeq.map(Token(_))) ===
        Exp.B('$', Exp.B('$', Exp.L(2, Exp.L(3, Exp.v(2))), Exp.B('.', Exp.S("Hello"), Exp.S(" World!"))), Exp.I(42))
    )

    println(Exp.show(Exp("B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK".split(" ").toSeq.map(Token(_)))))
  }