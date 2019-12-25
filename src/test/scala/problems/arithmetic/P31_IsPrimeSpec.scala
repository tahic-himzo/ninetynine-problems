package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.ArithmeticProblems._

class P31_IsPrimeSpec extends WordSpec with Matchers {
  "P31_IsPrime" should {
    "check if an integer is prime" in {
      forAll(
        Table(
          ("number", "is_prime"),
          (0, false),
          (1, false),
          (2, true),
          (3, true),
          (4, false),
          (5, true),
          (6, false),
          (7, true),
          (8, false),
          (9, false),
          (10, false),
          (11, true),
          (12, false),
          (100003, true),
          (100004, false)
        )
      ) {
        case (n, isPrime) => n.isPrime shouldEqual isPrime
      }
    }
  }
}
