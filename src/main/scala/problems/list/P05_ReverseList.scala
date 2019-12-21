package problems.list

object P05_ReverseList {

  //  Reverse the elements in a list.
  def execute[A](list: List[A]): List[A] = list.foldLeft(List.empty[A]) {
    case (revertedList, item) => item :: revertedList
  }
}
