package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.SolutionsToTheArithmeticProblems._

import scala.util.{Failure, Try}

class P41_PrintGoldbachList extends WordSpec with Matchers {
  "P40_PrintGoldbachList" should {
    "print the two prime numbers that sum up to any given even integer in a range" in {
      3.printGoldbachList(100)
    }
  }
}
