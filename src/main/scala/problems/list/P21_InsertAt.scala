package problems.list

object P21_InsertAt {

  //  Insert an item at the n-th index of a list.
  def execute[A](item: A, insertionIndex: Int, list: List[A]): List[A] =
    (insertionIndex, list.length) match {
      case (targetIndex, len) if len < targetIndex =>
        throw new IllegalArgumentException("Please provide an index that is <= the list length.")
      case (targetIndex, len) if targetIndex == len => list.appended(item)
      case (targetIndex, _) if targetIndex < 0      => throw new IllegalArgumentException("Please provide a positive index.")
      case (targetIndex, _) =>
        list.zipWithIndex
          .foldLeft(List.empty[A]) {
            case (newList, (currentItem, currentIndex)) if currentIndex == targetIndex => currentItem :: item :: newList
            case (newList, (currentItem, _))                                           => currentItem :: newList
          }
          .reverse
    }

}
