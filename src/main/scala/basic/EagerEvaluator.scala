package basic

object EagerEvaluator:
  def evaluate(exp: Exp): Exp =
    println(Exp.show(exp))
    val result = exp match
      case Exp.Bool(b) => Exp.Bool(b)
      case Exp.I(value) => Exp.I(value)
      case Exp.S(value) => Exp.S(value)
      case Exp.U(op, a) => evaluateUnary(op, evaluate(a))
      case Exp.B('$', lambda, arg) =>
        evaluate(lambda) match
          case Exp.L(idx, body) => evaluate(substitute(body, idx, evaluate(arg))) // CBV
      case Exp.B(op, a1, a2) => evaluateBinary(op, evaluate(a1), evaluate(a2))
      case Exp.L(idx, body) => Exp.L(idx, body)
      case Exp.v(idx) => Exp.v(idx)
      case Exp.If(cond, ok, no) =>
        evaluate(cond) match
          case Exp.Bool(b) => if b then evaluate(ok) else evaluate(no)
    result

  def enc(value: String): String = "S" + value.map(REV(_) + BASE).map(_.toChar).mkString

  private val BASE = '!'
  private val CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n".toIndexedSeq
  private val REV: Map[Char, Int] = CHARS.zipWithIndex.toMap

  private def evaluateUnary(op: Char, a: Exp): Exp =
    (op, a) match
      case ('-', Exp.I(value)) => Exp.I(-value)
      case ('!', Exp.Bool(b)) => Exp.Bool(!b)
      case ('#', Exp.S(value)) => Exp.I(value.map(REV(_)).foldLeft(BigInt(0))((acc, v) => acc * 94 + v))
      case ('$', Exp.I(value)) => Exp.S(Seq.unfold(value)(c => if c > 0 then Some((CHARS((c % 94).toInt) -> c / 94)) else None).reverse.mkString)

  private def evaluateBinary(op: Char, a1: Exp, a2: Exp): Exp =
    (op, a1, a2) match
      case ('+', Exp.I(value1), Exp.I(value2)) => Exp.I(value1 + value2)
      case ('-', Exp.I(value1), Exp.I(value2)) => Exp.I(value1 - value2)
      case ('*', Exp.I(value1), Exp.I(value2)) => Exp.I(value1 * value2)
      case ('/', Exp.I(value1), Exp.I(value2)) => Exp.I(value1 / value2)
      case ('%', Exp.I(value1), Exp.I(value2)) => Exp.I(value1 % value2)
      case ('<', Exp.I(value1), Exp.I(value2)) => Exp.Bool(value1 < value2)
      case ('>', Exp.I(value1), Exp.I(value2)) => Exp.Bool(value1 > value2)
      case ('=', Exp.I(value1), Exp.I(value2)) => Exp.Bool(value1 == value2)
      case ('=', Exp.S(value1), Exp.S(value2)) => Exp.Bool(value1 == value2)
      case ('=', Exp.Bool(value1), Exp.Bool(value2)) => Exp.Bool(value1 == value2)
      case ('|', Exp.Bool(value1), Exp.Bool(value2)) => Exp.Bool(value1 | value2)
      case ('&', Exp.Bool(value1), Exp.Bool(value2)) => Exp.Bool(value1 & value2)
      case ('.', Exp.S(value1), Exp.S(value2)) => Exp.S(value1 + value2)
      case ('T', Exp.I(num), Exp.S(str)) => Exp.S(str.take(num.toInt))
      case ('D', Exp.I(num), Exp.S(str)) => Exp.S(str.drop(num.toInt))

  private def substitute(body: Exp, idx: Int, bound: Exp): Exp =
    body match
      case Exp.Bool(b) => Exp.Bool(b)
      case Exp.I(value) => Exp.I(value)
      case Exp.S(value) => Exp.S(value)
      case Exp.U(op, a) => Exp.U(op, substitute(a, idx, bound))
      case Exp.B(op, a1, a2) => Exp.B(op, substitute(a1, idx, bound), substitute(a2, idx, bound))
      case Exp.L(idx_, body) if idx_ == idx => Exp.L(idx, body) // shadowing?
      case Exp.L(idx_, body) => Exp.L(idx_, substitute(body, idx, bound))
      case Exp.v(idx_) if idx_ == idx => bound
      case Exp.v(idx_) => Exp.v(idx_)
      case Exp.If(cond, ok, no) => Exp.If(substitute(cond, idx, bound), substitute(ok, idx, bound), substitute(no, idx, bound))
