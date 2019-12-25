package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.ArithmeticProblems._

import scala.util.{Failure, Try}

class P40_GoldbachSpec extends WordSpec with Matchers {
  "P40_Goldbach" should {
    "find the two prime numbers that sum up to a given even integer" in {
      forAll(
        Table(
          ("number", "goldbachs"),
          (28, (5, 23)),
          (4, (2, 2))
        )
      ) {
        case (number, expectedGoldbach) => number.goldbach shouldEqual expectedGoldbach
      }
    }

    "throw an error if the input is less than 3" in {
      Try((-2).goldbach) shouldBe a[Failure[_]]
      Try(0.goldbach) shouldBe a[Failure[_]]
      Try(1.goldbach) shouldBe a[Failure[_]]
      Try(2.goldbach) shouldBe a[Failure[_]]
    }

    "throw an error if the input is uneven" in {
      Try(3.goldbach) shouldBe a[Failure[_]]
      Try(5.goldbach) shouldBe a[Failure[_]]
      Try(137.goldbach) shouldBe a[Failure[_]]
    }
  }
}
