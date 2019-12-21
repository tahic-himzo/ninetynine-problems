package problems.list

import org.scalatest._

class P06_ListPalindromeSpec extends WordSpec with Matchers {
  "P06_ListPalindrome" should {
    "check wether a list is a palindrome" in {
      P06_ListPalindrome.execute(List(1, 2, 3, 2, 1)) shouldEqual true
      P06_ListPalindrome.execute(List(1, 2, 3, 3, 1)) shouldEqual false
      P06_ListPalindrome.execute(List(1, 1)) shouldEqual true
      P06_ListPalindrome.execute(List(1)) shouldEqual true
      P06_ListPalindrome.execute(List()) shouldEqual true
    }
  }
}
