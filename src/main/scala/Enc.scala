import basic.*

@main def enc() =
  val line = java.util.Scanner(System.in).nextLine()
  println(Evaluator.enc(line))
