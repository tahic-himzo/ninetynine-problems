package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.ArithmeticProblems._

import scala.util.{Failure, Try}

@Ignore
class P41_PrintGoldbachList extends WordSpec with Matchers {
  "P41_PrintGoldbachList" should {
    "print the two prime numbers that sum up to any given even integer in a range" in {
      3.printGoldbachList(100)
    }
  }

  "P41_PrintGoldbachListLimited" should {
    "print the two prime numbers that sum up to any given even integer in a range if both of them are bigger than a given minimum" in {
      3.printGoldbachListLimited(2000, 50)
    }
  }
}
