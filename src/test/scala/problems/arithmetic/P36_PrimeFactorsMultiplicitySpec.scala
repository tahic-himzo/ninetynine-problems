package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P36_PrimeFactorsMultiplicitySpec extends WordSpec with Matchers {
  "P36_PrimeFactorsMultiplicity" should {
    "get the number of prime factors of a positive integer, grouped by occurence" in {
      forAll(
        Table(
          ("a", "primefactors"),
          (315, Map(3 -> 2, 5 -> 1, 7 -> 1)),
          (36, Map(2 -> 2, 3 -> 2)),
          (2, Map(2 -> 1)),
        )
      ) {
        case (a, expectedPrimeFactors) => a.primeFactorsMultiplicity shouldEqual expectedPrimeFactors
      }
    }

    "throw an error if the input is less than 2" in {
      Try((-2).primeFactorsMultiplicity) shouldBe a[Failure[_]]
      Try(0.primeFactorsMultiplicity) shouldBe a[Failure[_]]
      Try(1.primeFactorsMultiplicity) shouldBe a[Failure[_]]
    }
  }
}
