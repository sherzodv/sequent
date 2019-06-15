package sequent.untyped

import sequent.untyped.Parser.Implicits.stringToSym
import sequent.untyped.Parser.SymWrap

import scala.annotation.tailrec

object Church {

  object Num {

    implicit class RichChurchInt[Sym: SymWrap](a: Int) {
      @tailrec
      private def count(cnt: Int, num: Lam[Sym]): Lam[Sym] = {
        if (cnt == a) {
          num
        } else {
          count(cnt + 1, succ(num))
        }
      }
      def toChurch: Lam[Sym] = {
        count(0, _0)
      }
    }

    implicit class RichChurchNum[Sym](numR: Lam[Sym]) {
      @tailrec
      private def count(acc: Int, num: Lam[Sym]): Int = {
        num match {
          case Abs(_, body) => count(acc + 1, body)
          case _ => acc - 1
        }
      }
      def toInt: Int = count(-1, numR)
    }

    def succ[Sym](num: Lam[Sym])(implicit w: SymWrap[Sym]): Lam[Sym] = {
      Abs(w.create("f"), num)
    }

    def _0[Sym: SymWrap]: Lam[Sym] = Abs("f", Abs("x", Var("x")))
    def _1[Sym: SymWrap]: Lam[Sym] = succ(_0)
    def _2[Sym: SymWrap]: Lam[Sym] = succ(_1)
    def _3[Sym: SymWrap]: Lam[Sym] = succ(_2)
    def _4[Sym: SymWrap]: Lam[Sym] = succ(_3)
    def _5[Sym: SymWrap]: Lam[Sym] = succ(_4)
    def _6[Sym: SymWrap]: Lam[Sym] = succ(_5)
    def _7[Sym: SymWrap]: Lam[Sym] = succ(_6)
    def _8[Sym: SymWrap]: Lam[Sym] = succ(_7)
    def _9[Sym: SymWrap]: Lam[Sym] = succ(_8)
  }

}