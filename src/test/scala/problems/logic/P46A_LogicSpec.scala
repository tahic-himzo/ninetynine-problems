package problems.logic

import org.scalatest._
import org.scalatest.Inspectors.forAll
import problems.logic.LogicProblems._

class P46A_LogicSpec extends WordSpec with Matchers {
  "P46B_TruthTables" should {
    val input = List((true, true), (true, false), (false, true), (false, false))

    "return the correct result for AND" in assertCorrectness(and, List(true, false, false, false))

    "return the correct result for OR" in assertCorrectness(or, List(true, true, true, false))

    "return the correct result for NAND" in assertCorrectness(nand, List(false, true, true, true))

    "return the correct result for NOR" in assertCorrectness(nor, List(false, false, false, true))

    "return the correct result for XOR" in assertCorrectness(xor, List(false, true, true, false))

    "return the correct result for IMPl" in assertCorrectness(impl, List(true, false, true, true))

    "return the correct result for EQU" in assertCorrectness(equ, List(true, false, false, true))

    def assertCorrectness(f: (Boolean, Boolean) => Boolean, expected: List[Boolean]): Assertion =
      forAll(input.zip(expected)) {
        case ((a, b), exp) => f(a, b) shouldEqual exp
      }
  }
}
