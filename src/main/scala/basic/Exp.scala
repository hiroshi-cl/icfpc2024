package basic

enum Exp:
  case Bool(b: Boolean)
  case I(value: BigInt)
  case S(value: String)
  case U(op: Char, a: Exp)
  case B(op: Char, a1: Exp, a2: Exp)
  case L(idx: Int, body: Exp)
  case v(idx: Int)
  case If(cond: Exp, ok: Exp, no: Exp)

object Exp:
  def apply(tokens: Seq[Token]): Exp =
    tokens.foldRight(Seq.empty[Exp]) {
      case (Token.T, seq) => Exp.Bool(true) +: seq
      case (Token.F, seq) => Exp.Bool(false) +: seq
      case (Token.I(value), seq) => Exp.I(value) +: seq
      case (Token.S(value), seq) => Exp.S(value) +: seq
      case (Token.U(op), a +: seq) => Exp.U(op, a) +: seq
      case (Token.B(op), a1 +: a2 +: seq) => Exp.B(op, a1, a2) +: seq
      case (Token.L(idx), body +: seq) => Exp.L(idx, body) +: seq
      case (Token.v(idx), seq) => Exp.v(idx) +: seq
      case (Token.If, cond +: ok +: no +: seq) => Exp.If(cond, ok, no) +: seq
    }.head

  def show(exp: Exp): String =
    exp match
      case Exp.Bool(b) => b.toString
      case Exp.I(value) => value.toString
      case Exp.S(value) => "\"" + value + "\""
      case Exp.U(op, a) => "(" + op + " " + show(a) + ")"
      case Exp.B(op, a1, a2) => "(" + show(a1) + " " + op + " " + show(a2) + ")"
      case Exp.L(idx, body) => "(\\v" + idx.toString + " -> " + show(body) + ")"
      case Exp.v(idx) => "v" + idx.toString
      case Exp.If(cond, ok, no) => "(if " + show(cond) + " then " + show(ok) + " else " + show(no) + ")"
