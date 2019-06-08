package sequent.untyped

import atto._
import Atto._
import cats.syntax.apply._
import scala.annotation.unchecked.uncheckedVariance

object Parser {
  type P[A] = atto.Parser[A]

  trait SymCreator[Sym] {
    def unit: Sym
    def create(name: String): Sym
  }

  def create[Sym](implicit symCreator: SymCreator[Sym]): String => Either[String, Lam[Sym]] = {
    lazy val sym: P[Sym] = oneOf(('a' to 'z').mkString).map(x => symCreator.create(x.toString))
    lazy val lam: P[Lam[Sym]] = abs or app or war or parens(lam)
    lazy val war: P[Var[Sym]] = sym.map(Var(_))
    lazy val abs: P[Abs[Sym]] = (char('/') ~> sym, char('.') ~> lam).mapN(Abs[Sym])
    lazy val app: P[App[Sym]] = (wam, many1(char(' ')) ~> lam).mapN(App[Sym])
    lazy val wam: P[Lam[Sym]] = abs or war or parens(wam) or parens(app)
    proof: String => {
      val r = lam.parse(proof)
        .done
      r.either
    }
  }

  def parse[Sym](proof: String)(implicit symCreator: SymCreator[Sym]): Either[String, Lam[Sym]] = {
    create(symCreator)(proof)
  }

  private implicit class CovariantParser[+A](val inv: P[A] @uncheckedVariance) {
    def or[C >: A](other: => CovariantParser[C]): CovariantParser[C] = {
      inv | other.inv
    }
  }

  private implicit def covariantParserToInvariant[A](cp: CovariantParser[A]): P[A] = cp.inv

  object Implicits {
    implicit val StringSym: SymCreator[String] = new SymCreator[String] {
      def unit: String = ""
      def create(name: String): String = name
    }
  }
}
