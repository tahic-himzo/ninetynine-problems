package problems.list

object P11_ModifiedRunLength {

  //  Group consecutive duplicate elements in run-length encoding if multiple items, copy as-is if only 1 item.
  def execute[A](list: List[A]): List[Any] =
    P09_GroupConsecDuplicates
      .execute(list)
      .foldLeft(List.empty[Any]) {
        case (acc, Nil) => acc
        case (encoded, nextList @ (x :: _)) if nextList.length > 1 => (nextList.length, x) :: encoded

        case (encoded, x :: _) => x :: encoded
      }
      .reverse
}
