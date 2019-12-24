package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P32_GcdSpec extends WordSpec with Matchers {
  "P32_gcd" should {
    "find the GCD of two integers" in {
      forAll(
        Table(
          ("a", "b", "gcd"),
          (10, 20, 10),
          (5, 15, 5),
          (13, 17, 1),
          (36, 63, 9),
          (2, 2, 2),
          (1, 2, 1)
        )
      ) {
        case (a, b, expectedGCD) => a.gcd(b) shouldEqual expectedGCD
      }
    }

    "throw an error if one of the inputs is not positive" in {
      Try(0.gcd(5)) shouldBe a[Failure[_]]
      Try(0.gcd(0)) shouldBe a[Failure[_]]
      Try(5.gcd(0)) shouldBe a[Failure[_]]
    }
  }
}
