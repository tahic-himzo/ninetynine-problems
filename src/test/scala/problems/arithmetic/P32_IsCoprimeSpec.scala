package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P32_IsCoprimeSpec extends WordSpec with Matchers {
  "P32_IsCoprime" should {
    "find out if two integers are coprime" in {
      forAll(
        Table(
          ("a", "b", "isCoprime"),
          (10, 20, false),
          (5, 15, false),
          (13, 17, true),
          (36, 63, false),
          (2, 2, false),
          (1, 2, true),
          (35, 64, true)
        )
      ) {
        case (a, b, isCoprime) => a.isCoprimeTo(b) shouldEqual isCoprime
      }
    }

    "throw an error if one of the inputs is not positive" in {
      Try(0.gcd(5)) shouldBe a[Failure[_]]
      Try(0.gcd(0)) shouldBe a[Failure[_]]
      Try(5.gcd(0)) shouldBe a[Failure[_]]
    }
  }
}
