package problems.list

import org.scalatest._

class P09_GroupConsecDuplicatesSpec extends WordSpec with Matchers {
  "P09_GroupConsecDuplicates" should {
    "group consecutive duplicates" in {
      P09_GroupConsecDuplicates.execute(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")) shouldEqual List(
        List("a", "a", "a", "a"),
        List("b"),
        List("c", "c"),
        List("a", "a"),
        List("d"),
        List("e", "e", "e", "e"))
      P09_GroupConsecDuplicates.execute(List("a", "a")) shouldEqual List(List("a", "a"))
      P09_GroupConsecDuplicates.execute(List()) shouldEqual List()
    }
  }
}
