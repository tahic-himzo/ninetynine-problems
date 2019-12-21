package problems.list

import org.scalatest._

class P12_DecodeRunLengthSpec extends WordSpec with Matchers {
  "P12_DecodeRunLength" should {
    "decode a run length encoded list" in {
      P12_DecodeRunLength.execute(List(
        (4, "a"),
        (1, "b"),
        (2, "c"),
        (2, "a"),
        (1, "d"),
        (4, "e"))) shouldEqual List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")
      P12_DecodeRunLength.execute(List((2, "a"))) shouldEqual List("a", "a")
      P12_DecodeRunLength.execute(List((1, "a"), (1, "b"))) shouldEqual List("a", "b")
      P12_DecodeRunLength.execute(List()) shouldEqual List()
    }
  }
}
