package spaceship

import basic.*
import sttp.client3.*
import sttp.model.Header

@main def run(): Unit =
  val backend = HttpClientSyncBackend()
  val sc = java.util.Scanner(System.in)
  val endpoint = basicRequest
    .headers(Header.authorization("Bearer", System.getProperty("icfpc.api_key")))
    .post(uri"https://boundvariable.space/communicate")

  while sc.hasNext do
    val idx = sc.next()
    val command = Evaluator.enc(s"get spaceship$idx")
    val response = endpoint.body(command, "utf-8").send(backend)
    response.body match
      case Left(value) => throw RuntimeException(value)
      case Right(value) =>
        val tokens = value.split(" ").toSeq.map(Token(_))
        val inputText = Evaluator.evaluate(Exp(tokens)).asInstanceOf[Exp.S].value
        println(inputText)
        val solution = Solver.solve(inputText.split("\n").map(_.split(" ").toSeq match
          case x +: y +: _ => (x.toInt, y.toInt)
        ))
        println(solution)
        val response = endpoint.body(Evaluator.enc(s"solve spaceship$idx ${solution.mkString}"), "utf-8").send(backend)
        response.body match
          case Left(value) => throw RuntimeException(value)
          case Right(value) =>
            val tokens = value.split(" ").toSeq.map(Token(_))
            println(Evaluator.evaluate(Exp(tokens)).asInstanceOf[Exp.S].value)
