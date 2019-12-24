package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P34_TotientSpec extends WordSpec with Matchers {
  "P34_Totient" should {
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
        case (a, expectedTotient) => a.totient shouldEqual expectedTotient
      }
    }

    "throw an error if the input is not positive" in {
      Try(-2.totient) shouldBe a[Failure[_]]
      Try(0.totient) shouldBe a[Failure[_]]
    }
  }
}
