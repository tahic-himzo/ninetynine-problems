package problems.list

object P08_EliminateConsecDuplicates {

  //  Eliminate consecutive duplicate elements in a list
  def execute[A](list: List[A]): List[A] =
    list
      .foldLeft(List.empty[A]) {
        case (listSoFar, nextItem) if listSoFar.headOption.contains(nextItem) => listSoFar
        case (listSoFar, nextItem)                                            => nextItem :: listSoFar
      }.reverse
}
