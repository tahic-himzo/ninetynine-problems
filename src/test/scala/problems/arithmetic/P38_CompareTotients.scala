package problems.arithmetic

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import problems.arithmetic.ArithmeticProblems._

import scala.util.{Failure, Try}

@Ignore
class P38_CompareTotients extends WordSpec with Matchers {
  "P38_CompareTotients" should {
    "return a string describing the performance of the two totient methods" in {
      val comparison = 10090.compareTotients
      println(comparison)
    }
  }
}
