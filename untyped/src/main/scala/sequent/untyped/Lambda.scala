package sequent.untyped

sealed trait Lam[Sym]
final case class Var[Sym](name: Sym) extends Lam[Sym]
final case class Abs[Sym](name: Sym, body: Lam[Sym]) extends Lam[Sym]
final case class App[Sym](abs: Lam[Sym], arg: Lam[Sym]) extends Lam[Sym]

object Norm {

  def alpha[Sym](range: List[Sym])(ast: Lam[Sym]): Lam[Sym] = ???

}
