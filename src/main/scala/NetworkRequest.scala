import basic.*
import sttp.client3.*
import sttp.model.Header

@main def request(): Unit =
  val backend = HttpClientSyncBackend()
  val sc = java.util.Scanner(System.in)
  val endpoint = basicRequest
    .headers(Header.authorization("Bearer", System.getProperty("icfpc.api_key")))
    .post(uri"https://boundvariable.space/communicate")

  while sc.hasNextLine do
    val command = Evaluator.enc(sc.nextLine())
    val response = endpoint.body(command, "utf-8").send(backend)
    response.body match
      case Left(value) => throw RuntimeException(value)
      case Right(value) =>
        val tokens = value.split(" ").toSeq.map(Token(_))
        println(tokens)
        val exp = Exp(tokens)
        val evaluated = Evaluator.evaluate(exp)
        println(evaluated)
