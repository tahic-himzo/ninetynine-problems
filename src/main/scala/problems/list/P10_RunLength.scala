package problems.list

object P10_RunLength {

  //  Group consecutive duplicate elements in run-length encoding
  def execute[A](list: List[A]): List[(Int, A)] =
    P09_GroupConsecDuplicates
      .execute(list)
      .foldLeft(List.empty[(Int, A)]) {
        case (acc, Nil) => acc
        case (encoded, nextList @ (x :: _)) => (nextList.length, x) :: encoded
      }
      .reverse
}
