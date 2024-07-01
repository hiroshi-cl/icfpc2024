import basic.*

@main def dec() =
  val line = java.util.Scanner(System.in).nextLine()
  val tokens = line.split(" ").toSeq.map(Token(_))
  val exp = Exp(tokens)
  println(tokens)
  println(Exp.show(exp))
  val evaluated = Evaluator.evaluate(exp)
  println(Exp.show(evaluated))
