package problems.logic

import problems.logic.LogicProblems._
import org.scalatest._

class P46_TruthTablesSpec extends WordSpec with Matchers {
  "P46_TruthTables" should {
    "return the truth tables for a given boolean expression" in {
      createTruthTable(and) shouldEqual
      """|A     B     result
         |true  true  true
         |true  false false
         |false true  false
         |false false false""".stripMargin

      createTruthTable((a, b) => and(a, or(a, b)))
    }
  }
}
