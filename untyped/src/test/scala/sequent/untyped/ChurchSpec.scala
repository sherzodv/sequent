package sequent.untyped

import org.scalatest._

class ChurchSpec extends FlatSpec with Matchers {

  import Church.Num._
  import Implicits.StringIdx

  def swap[A, B](a: (A, B)): (B, A) = (a._2, a._1)

  "Church numbers" must "be convertible to Scala integers and vice versa" in {
    val nums = List(
      _0 -> 0,
      _1 -> 1,
      _2 -> 2,
      _3 -> 3,
      _4 -> 4,
      _5 -> 5,
      _6 -> 6,
      _7 -> 7,
      _8 -> 8,
      _9 -> 9,
    )
    nums.map { case (cn, sn) => cn.toInt shouldBe sn }
    nums.map(swap).map { case (sn, cn) => sn.toChurch shouldBe cn }
  }

}
