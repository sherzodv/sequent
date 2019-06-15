package sequent.untyped

/** Wrapper to abstract over different indexation methods.
  */
trait Idx[I] {
  def unit: I
  def create(name: String): I
}

sealed trait Lam[I]
final case class Var[I](name: I) extends Lam[I]
final case class Abs[I](name: I, body: Lam[I]) extends Lam[I]
final case class App[I](abs: Lam[I], arg: Lam[I]) extends Lam[I]

object Norm {

  def alpha[I](range: List[I])(ast: Lam[I]): Lam[I] = ???

}

object Implicits {
  implicit val StringIdx: Idx[String] = new Idx[String] {
    def unit: String = ""
    def create(name: String): String = name
  }
  implicit def stringToIdx[I](s: String)(implicit w: Idx[I]): I = w.create(s)
}
