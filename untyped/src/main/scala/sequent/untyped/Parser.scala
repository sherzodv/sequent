package sequent.untyped

import atto._
import Atto._
import cats.syntax.apply._

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

object Parser {
  type P[A] = atto.Parser[A]
  type R[A] = Either[String, A]

  /** Name wrapper to abstract over different indexation methods.
    */
  trait SymWrap[Sym] {
    def unit: Sym
    def create(name: String): Sym
  }

  def create[Sym](implicit symWrap: SymWrap[Sym]): String => R[Lam[Sym]] = {
    lazy val sym: P[Sym] = oneOf(('a' to 'z').mkString).map(x => symWrap.create(x.toString))
    lazy val lam: P[Lam[Sym]] = abs or app or war or parens(lam)
    lazy val war: P[Var[Sym]] = sym.map(Var(_))
    lazy val abs: P[Abs[Sym]] = ((char('/') | char('λ')) ~> sym, char('.') ~> lam).mapN(Abs[Sym])
    lazy val app: P[App[Sym]] = (wam, many1(char(' ')) ~> lam).mapN(App[Sym])
    lazy val wam: P[Lam[Sym]] = abs or war or parens(wam) or parens(app)
    proof: String => {
      val r = lam.parse(proof)
        .done
      r.either
    }
  }

  def parse[Sym](proof: String)(implicit symWrap: SymWrap[Sym]): R[Lam[Sym]] = {
    create(symWrap)(proof)
  }

  private implicit class CovariantParser[+A](val inv: P[A] @uncheckedVariance) {
    def or[C >: A](other: => CovariantParser[C]): CovariantParser[C] = {
      inv | other.inv
    }
  }

  private implicit def covariantParserToInvariant[A](cp: CovariantParser[A]): P[A] = cp.inv

  object Implicits {
    implicit val StringSym: SymWrap[String] = new SymWrap[String] {
      def unit: String = ""
      def create(name: String): String = name
    }
    implicit def stringToSym[Sym](s: String)(implicit w: SymWrap[Sym]): Sym = w.create(s)
    implicit class LambdaAst(val s: StringContext) extends AnyVal {
      def L[Sym: SymWrap](args: Any*): R[Lam[Sym]] = parse(s.s(args: _*))
    }
    implicit class LambdaCode[Sym: ClassTag](val ast: Lam[Sym]) {
      def code: String = code(ast)
      private def code(a: Lam[Sym]): String = {
        a match {
          case a: Var[Sym] => code(a)
          case a: Abs[Sym] => code(a)
          case a: App[Sym] => code(a)
        }
      }
      private def code(a: Var[Sym]): String = a.name.toString
      private def code(a: Abs[Sym]): String = s"λ${a.name.toString}.${code(a.body)}"
      private def code(a: App[Sym]): String = (a.abs, a.arg) match {
        case (x: Var[Sym], y: Var[Sym]) => code(x) + " " + code(y)
        case (x, y: Var[Sym])           => "(" + code(x) + ") " + code(y)
        case (x, y)                     => "(" + code(x) + ") (" + code(y) + ")"
      }
    }
  }
}
