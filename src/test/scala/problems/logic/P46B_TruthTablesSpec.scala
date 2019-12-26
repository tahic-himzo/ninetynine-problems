package problems.logic

import problems.logic.LogicProblems._
import org.scalatest._

class P46B_TruthTablesSpec extends WordSpec with Matchers {
  "P46B_TruthTables" should {
    "return the correct truth tables for a given boolean expression" in {
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
