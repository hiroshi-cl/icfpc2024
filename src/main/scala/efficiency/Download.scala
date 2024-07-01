package efficiency

import basic.*
import sttp.client3.*
import sttp.model.Header

@main def download(): Unit =
  val backend = HttpClientSyncBackend()
  val sc = java.util.Scanner(System.in)
  val endpoint = basicRequest
    .headers(Header.authorization("Bearer", System.getProperty("icfpc.api_key")))
    .post(uri"https://boundvariable.space/communicate")

  for i <- 1 to 13 do
    val command = Evaluator.enc(s"get efficiency$i")
    val response = endpoint.body(command, "utf-8").send(backend)
    response.body match
      case Left(value) => throw RuntimeException(value)
      case Right(value) =>
        val tokens = value.split(" ").toSeq.map(Token(_))
        println(i + ":")
        println(tokens)
        val exp = Exp(tokens)
        println(Exp.show(exp))
