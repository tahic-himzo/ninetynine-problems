package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P34_PrimeFactorsSpec extends WordSpec with Matchers {
  "P34_PrimeFactors" should {
    "get the number of prime factors of a positive integer" in {
      forAll(
        Table(
          ("a", "primefactors"),
          (315, List(3, 3, 5, 7)),
          (36, List(2, 2, 3, 3)),
          (2, List(2)),
        )
      ) {
        case (a, expectedPrimeFactors) => a.primeFactors shouldEqual expectedPrimeFactors
      }
    }

    "throw an error if the input is less than 2" in {
      Try((-2).primeFactors) shouldBe a[Failure[_]]
      Try(0.primeFactors) shouldBe a[Failure[_]]
      Try(1.primeFactors) shouldBe a[Failure[_]]
    }
  }
}
