package sequent.untyped

import atto._
import Atto._
import cats.syntax.apply._

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

object Parser {
  type P[A] = atto.Parser[A]
  type R[A] = Either[String, A]

  def create[I](implicit idx: Idx[I]): String => R[Lam[I]] = {
    lazy val sym: P[I] = oneOf(('a' to 'z').mkString).map(x => idx.create(x.toString))
    lazy val lam: P[Lam[I]] = abs or app or war or parens(lam)
    lazy val war: P[Var[I]] = sym.map(Var(_))
    lazy val abs: P[Abs[I]] = ((char('/') | char('λ')) ~> sym, char('.') ~> lam).mapN(Abs[I])
    lazy val app: P[App[I]] = (wam, many1(char(' ')) ~> lam).mapN(App[I])
    lazy val wam: P[Lam[I]] = abs or war or parens(wam) or parens(app)
    proof: String => {
      val r = lam.parse(proof)
        .done
      r.either
    }
  }

  def parse[I](proof: String)(implicit idx: Idx[I]): R[Lam[I]] = {
    create(idx)(proof)
  }

  private implicit class CovariantParser[+A](val inv: P[A] @uncheckedVariance) {
    def or[C >: A](other: => CovariantParser[C]): CovariantParser[C] = {
      inv | other.inv
    }
  }

  private implicit def covariantParserToInvariant[A](cp: CovariantParser[A]): P[A] = cp.inv

  object Implicits {
    implicit class LambdaAst(val s: StringContext) extends AnyVal {
      def L[I: Idx](args: Any*): R[Lam[I]] = parse(s.s(args: _*))
    }
    implicit class LambdaCode[I: ClassTag](val ast: Lam[I]) {
      def code: String = code(ast)
      private def code(a: Lam[I]): String = {
        a match {
          case a: Var[I] => code(a)
          case a: Abs[I] => code(a)
          case a: App[I] => code(a)
        }
      }
      private def code(a: Var[I]): String = a.name.toString
      private def code(a: Abs[I]): String = s"λ${a.name.toString}.${code(a.body)}"
      private def code(a: App[I]): String = (a.abs, a.arg) match {
        case (x: Var[I], y: Var[I]) => code(x) + " " + code(y)
        case (x, y: Var[I])           => "(" + code(x) + ") " + code(y)
        case (x, y)                     => "(" + code(x) + ") (" + code(y) + ")"
      }
    }
  }
}
