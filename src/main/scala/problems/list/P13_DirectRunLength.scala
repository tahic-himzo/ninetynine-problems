package problems.list

object P13_DirectRunLength {

  //  Group consecutive duplicate elements in run-length encoding
  def execute[A](list: List[A]): List[(Int, A)] =
    list.foldLeft(List.empty[(Int, A)]) {
      case ((lastCount, lastItem) :: encodedSoFar, currentItem) if lastItem == currentItem =>
        (lastCount + 1, lastItem) :: encodedSoFar
      case (encodedSoFar, currentItem) =>
        (1, currentItem) :: encodedSoFar
    }.reverse
}
