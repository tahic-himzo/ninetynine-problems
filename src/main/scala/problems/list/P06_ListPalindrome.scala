package problems.list

object P06_ListPalindrome {

  //  Find out whether a list is a palindrome
  def execute[A](list: List[A]): Boolean =
    list.length match {
      case len =>
        list
          .foldLeft((List.empty[A], 0: Int, true: Boolean)) {
            case ((leftHalf, i, _), currentItem) if i < len / 2 => (currentItem :: leftHalf, i + 1, true)
            case ((leftHalf, i, _), _) if i == len / 2 && len % 2 == 1 => (leftHalf, i + 1, true)
            case ((x :: xs, i, isPalin), currentItem) if x == currentItem => (xs, i + 1, isPalin)
            case ((_ :: xs, i, _), _)                                     => (xs, i + 1, false)
          }
          ._3
    }
}
