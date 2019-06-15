package sequent.untyped

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

  import Implicits.StringIdx

  "Parser" must "parse free variable" in {
    Parser.parse("x") shouldBe Right(
      Var("x")
    )
  }

  "Parser" must "parse abstraction" in {
    Parser.parse("λx.x") shouldBe Right(
      Abs("x", Var("x"))
    )
  }

  "Parser" must "parse application" in {
    Parser.parse("x x") shouldBe Right(
      App(Var("x"), Var("x"))
    )
  }

  "Parser" must "parse application inside abstraction" in {
    Parser.parse("λx.x y") shouldBe Right(
      Abs("x", App(Var("x"), Var("y")))
    )
  }

  "Parser" must "parse sequential applications" in {
    Parser.parse("x y z") shouldBe Right(
      App(Var("x"), App(Var("y"), Var("z")))
    )
  }

  "Parser" must "parse parenthesized terms" in {
    Parser.parse("(x) (y) (z)") shouldBe Right(
      App(Var("x"), App(Var("y"), Var("z")))
    )
  }

  "Parser" must "take parentheses as association" in {
    Parser.parse("(x y) z") shouldBe Right(
      App(App(Var("x"), Var("y")), Var("z"))
    )
  }

  "Parser" must "parse parenthesized abstraction application" in {
    Parser.parse("(λx.x y) z") shouldBe Right(
      App(
        Abs(
          "x",
          App(Var("x"), Var("y"))
        ),
        Var("z")
      )
    )
  }

  "Parser" must "parse abstraction in abstraction" in {
    Parser.parse("λf.λx.x") shouldBe Right(
      Abs(
        "f",
        Abs(
          "x",
          Var("x")
        )
      )
    )
  }
}
