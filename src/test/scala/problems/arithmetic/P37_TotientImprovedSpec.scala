package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.ArithmeticProblems._

import scala.util.{Failure, Try}

class P37_TotientImprovedSpec extends WordSpec with Matchers {
  "P34_TotientImproved" should {
    "get the number of all coprimes between 1 and a given value" in {
      forAll(
        Table(
          ("a", "totient"),
          (10, 4),
          (123456, 41088),
          (1, 1),
          (2, 1)
        )
      ) {
        case (a, expectedTotient) => a.totientImproved shouldEqual expectedTotient
      }
    }

    "throw an error if the input is not positive" in {
      Try(-2.totientImproved) shouldBe a[Failure[_]]
      Try(0.totientImproved) shouldBe a[Failure[_]]
    }
  }
}
