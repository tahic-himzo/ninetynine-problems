package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P24_SampleARangeSpec extends WordSpec with Matchers {
  "P24_SampleARange" should {
    "extract n items of a range at random" in {
      testLengthAndDataIntersection(3, 6)
      testLengthAndDataIntersection(6, 49)
      testLengthAndDataIntersection(2, -5)
    }

    "fail if n is negative" in {
      Try(P24_SampleARange.execute(-1, 3)) shouldBe a[Failure[_]]
    }

    "fail if range is 1" in {
      Try(P24_SampleARange.execute(1, 1)) shouldBe a[Failure[_]]
    }

    "fail if n is bigger than range" in {
      Try(P24_SampleARange.execute(4, 3)) shouldBe a[Failure[_]]
    }

    def testLengthAndDataIntersection(n: Int, range: Int): Assertion = {
      val output = P24_SampleARange.execute(n, range)
      output.length shouldEqual n
      Inspectors.forAll(output)(
        value => {
          if(range < 1) {
            value <= 1 shouldBe true
            value >= range shouldEqual true
          } else {
            value >= 1 shouldBe true
            value <= range shouldEqual true
          }
        }
      )
    }
  }
}
