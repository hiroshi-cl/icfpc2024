package basic

import org.scalatest.funsuite.AnyFunSuiteLike

class EvaluatorTest extends AnyFunSuiteLike:

  def evaluate(str: String): Exp =
    Evaluator.evaluate(Exp(str.split(" ").toSeq.map(Token(_))))

  test("unary") {
    assert(evaluate("U- I$") === Exp.I(-3))
    assert(evaluate("U! T") === Exp.Bool(false))
    assert(evaluate("U# S4%34") === Exp.I(15818151))
    assert(evaluate("U$ I4%34") === Exp.S("test"))
  }

  test("binary") {
    assert(evaluate("B+ I# I$") === Exp.I(5))
    assert(evaluate("B* I$ I#") === Exp.I(6))
    assert(evaluate("B/ U- I( I#") === Exp.I(-3))
    assert(evaluate("B% U- I( I#") === Exp.I(-1))
    assert(evaluate("B< I$ I#") === Exp.Bool(false))
    assert(evaluate("B> I$ I#") === Exp.Bool(true))
    assert(evaluate("B= I$ I#") === Exp.Bool(false))
    assert(evaluate("B| T F") === Exp.Bool(true))
    assert(evaluate("B& T F") === Exp.Bool(false))
    assert(evaluate("B. S4% S34") === Exp.S("test"))
    assert(evaluate("BT I$ S4%34") === Exp.S("tes"))
    assert(evaluate("BD I$ S4%34") === Exp.S("t"))
  }

  test("lambda") {
    assert(evaluate("B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK") === Exp.S("Hello World!"))
  }

  test("if") {
    assert(evaluate("? B> I# I$ S9%3 S./") === Exp.S("no"))
  }

  test("eval") {
    assert(evaluate("B$ L# B$ L\" B+ v\" v\" B* I$ I# v8") === Exp.I(12))
  }

  test("big int") {
    assert(evaluate("U$ U# S_test_test_test_test_test_test_test_test_test") === evaluate("S_test_test_test_test_test_test_test_test_test"))
    assert(evaluate("U# U$ I_test_test_test_test_test_test_test_test_test") === evaluate("I_test_test_test_test_test_test_test_test_test"))
  }