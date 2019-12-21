package problems.list

object P14_P15_DuplicateElements {

  //  Duplicate all elements in a list n times.
  def execute[A](n: Int, list: List[A]): List[A] =
    list
      .foldLeft(List.empty[A]) {
        case (duplicated, currentItem) => List.fill(n)(currentItem) ::: duplicated
      }
      .reverse
}
