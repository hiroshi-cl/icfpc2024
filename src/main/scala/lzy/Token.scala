package lzy

enum Token:
  case T
  case F
  case I(value: BigInt)
  case S(value: String)
  case U(op: Char)
  case B(op: Char)
  case L(idx: Int)
  case v(idx: Int)
  case If

object Token:
  private val BASE = '!'
  private val CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n".toIndexedSeq

  def apply(tokenStr: String): Token =
    tokenStr.toSeq match
      case 'T' +: _ => T
      case 'F' +: _ => F
      case 'I' +: valueStr => I(valueStr.map(_ - BASE).foldLeft(BigInt(0))((acc, v) => acc * 94 + v))
      case 'S' +: valueStr => S(valueStr.map(_ - BASE).map(CHARS(_)).mkString)
      case 'U' +: op +: _ => U(op)
      case 'B' +: op +: _ => B(op)
      case 'L' +: idx +: _ => L(idx - BASE)
      case 'v' +: idx +: _ => v(idx - BASE)
      case '?' +: _ => If