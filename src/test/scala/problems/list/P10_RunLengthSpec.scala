package problems.list

import org.scalatest._

class P10_RunLengthSpec extends WordSpec with Matchers {
  "P10_RunLength" should {
    "group consecutive duplicates in run length encoding" in {
      P10_RunLength.execute(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) shouldEqual List(
        (4, "a"),
        (1, "b"),
        (2, "c"),
        (2, "a"),
        (1, "d"),
        (4, "e"))
      P10_RunLength.execute(List("a", "a")) shouldEqual List((2, "a"))
      P10_RunLength.execute(List("a", "b")) shouldEqual List((1, "a"), (1, "b"))
      P10_RunLength.execute(List()) shouldEqual List()
    }
  }
}
