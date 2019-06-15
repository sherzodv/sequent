package sequent.untyped

import sequent.untyped.Implicits.stringToIdx

import scala.annotation.tailrec

object Church {

  object Num {

    implicit class RichChurchInt[I: Idx](a: Int) {
      @tailrec
      private def count(cnt: Int, num: Lam[I]): Lam[I] = {
        if (cnt == a) {
          num
        } else {
          count(cnt + 1, succ(num))
        }
      }
      def toChurch: Lam[I] = {
        count(0, _0)
      }
    }

    implicit class RichChurchNum[I](numR: Lam[I]) {
      @tailrec
      private def count(acc: Int, num: Lam[I]): Int = {
        num match {
          case Abs(_, body) => count(acc + 1, body)
          case _ => acc - 1
        }
      }
      def toInt: Int = count(-1, numR)
    }

    def succ[I](num: Lam[I])(implicit w: Idx[I]): Lam[I] = {
      Abs(w.create("f"), num)
    }

    def _0[I: Idx]: Lam[I] = Abs("f", Abs("x", Var("x")))
    def _1[I: Idx]: Lam[I] = succ(_0)
    def _2[I: Idx]: Lam[I] = succ(_1)
    def _3[I: Idx]: Lam[I] = succ(_2)
    def _4[I: Idx]: Lam[I] = succ(_3)
    def _5[I: Idx]: Lam[I] = succ(_4)
    def _6[I: Idx]: Lam[I] = succ(_5)
    def _7[I: Idx]: Lam[I] = succ(_6)
    def _8[I: Idx]: Lam[I] = succ(_7)
    def _9[I: Idx]: Lam[I] = succ(_8)
  }

}