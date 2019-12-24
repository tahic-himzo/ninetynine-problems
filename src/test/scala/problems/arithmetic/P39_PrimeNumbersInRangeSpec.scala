package problems.arithmetic

import org.scalatest._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P39_PrimeNumbersInRangeSpec extends WordSpec with Matchers {
  "P39_PrimeNumbersInRange" should {
    "return a list of prime numbers between 1 and the given value" in {
      7.listPrimesinRange(31) shouldEqual List(7, 11, 13, 17, 19, 23, 29, 31)
      2.listPrimesinRange(100) shouldEqual List(
        2,
        3,
        5,
        7,
        11,
        13,
        17,
        19,
        23,
        29,
        31,
        37,
        41,
        43,
        47,
        53,
        59,
        61,
        67,
        71,
        73,
        79,
        83,
        89,
        97)
      2.listPrimesinRange(2) shouldEqual List(2)
    }

    "throw an error if one of the values is not > 1" in {
      Try(1.listPrimesinRange(1)) shouldBe a[Failure[_]]
      Try(1.listPrimesinRange(2)) shouldBe a[Failure[_]]
      Try(0.listPrimesinRange(2)) shouldBe a[Failure[_]]
      Try(0.listPrimesinRange(1)) shouldBe a[Failure[_]]
    }

    "throw an error if the end is smaller than the begin" in {
      Try(5.listPrimesinRange(4)) shouldBe a[Failure[_]]
    }
  }
}
