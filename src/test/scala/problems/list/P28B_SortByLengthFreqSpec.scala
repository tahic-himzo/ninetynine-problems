package problems.list

import org.scalatest._

class P28B_SortByLengthFreqSpec extends WordSpec with Matchers {
  "P28A_SortByLengthFreq" should {
    "sort a list of lists by list length frequency" in {
      P28B_SortByLengthFreq.execute(
        List(
          List("a", "b", "c"),
          List("d", "e"),
          List("f", "g", "h"),
          List("d", "e"),
          List("i", "j", "k", "l"),
          List("m", "n"),
          List("o"))) shouldEqual List(
        List("i", "j", "k", "l"),
        List("o"),
        List("a", "b", "c"),
        List("f", "g", "h"),
        List("d", "e"),
        List("d", "e"),
        List("m", "n"))
    }
  }
}
