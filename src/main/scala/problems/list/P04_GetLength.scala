package problems.list

object P04_GetLength {

  //  Get the amount of elements in a list.
  def execute[A](list: List[A]): Int = list.foldLeft(0) {
    case (len, _) => len + 1
  }
}
