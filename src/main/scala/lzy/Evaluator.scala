package lzy

object Evaluator:
  def evaluate(exp: Exp): Exp =
    val result = exp match
      case e: Exp.Bool => e
      case e: Exp.I => e
      case e: Exp.S => e
      case Exp.U(op, a) => evaluateUnary(op, a.evaluated)
      case Exp.B('$', lambda, arg) =>
        lambda.evaluated match
          case Exp.L(idx, body, env) => substitute(body, env + (idx -> arg)).evaluated
      case Exp.B(op, a1, a2) => evaluateBinary(op, a1.evaluated, a2.evaluated)
      case e: Exp.L => e
      case e: Exp.v => e
      case Exp.If(cond, ok, no) =>
        cond.evaluated match
          case Exp.Bool(b) => if b then ok.evaluated else no.evaluated
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

  private def substitute(body: Exp, env: Map[Int, Exp]): Exp =
    body match
      case e: Exp.Bool => e
      case e: Exp.I => e
      case e: Exp.S => e
      case Exp.U(op, a) => Exp.U(op, substitute(a, env))
      case Exp.B(op, a1, a2) => Exp.B(op, substitute(a1, env), substitute(a2, env))
      case Exp.L(idx, body_, env_) => Exp.L(idx, body_, env_ ++ (env - idx)) // shadowing?
      case Exp.v(idx) => env.getOrElse(idx, Exp.v(idx))
      case Exp.If(cond, ok, no) => Exp.If(substitute(cond, env), substitute(ok, env), substitute(no, env))
