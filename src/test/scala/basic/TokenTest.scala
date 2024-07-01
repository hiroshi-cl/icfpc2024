package basic

class TokenTest extends org.scalatest.funsuite.AnyFunSuiteLike:

  test("apply") {
    assert(Token("T") === Token.T)
    assert(Token("F") === Token.F)
    assert(Token("I/6") === Token.I(1337))
    assert(Token("SB%,,/}Q/2,$_") === Token.S("Hello World!"))
    assert(Token("U$") === Token.U('$'))
    assert(Token("B+") === Token.B('+'))
    assert(Token("L#") === Token.L(2))
    assert(Token("v$") === Token.v(3))
    assert(Token("?") === Token.If)
  }