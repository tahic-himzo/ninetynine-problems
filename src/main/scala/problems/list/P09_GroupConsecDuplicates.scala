package problems.list

object P09_GroupConsecDuplicates {

  //  Group consecutive duplicate elements in sub-lists
  def execute[A](list: List[A]): List[List[A]] =
    list
      .foldLeft(List.empty: List[List[A]]) {
        case (listSoFar, nextItem) if listSoFar.nonEmpty && listSoFar.head.contains(nextItem) =>
          (nextItem :: listSoFar.head) :: listSoFar.tail
        case (listSoFar, nextItem) => List(nextItem) :: listSoFar
      }
      .reverse
}
