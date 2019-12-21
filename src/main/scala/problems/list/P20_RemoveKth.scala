package problems.list

object P20_RemoveKth {

  //  Remove the kth item from a list and return the new list and the remove item in a tuple.
  def execute[A](n: Int, list: List[A]): (List[A], A) = {
    if(n > list.length) throw new IllegalArgumentException("Please provide an index that is within the list boundaries.")
    val (outputList, removedItem) = list.zipWithIndex.foldLeft((List.empty[A], Option.empty[A])) {
      case ((resultList, None), (currentItem, currentIndex)) if currentIndex == n => (resultList, Some(currentItem))

      case ((resultList, maybeRemovedItem), (currentItem, _)) => (currentItem :: resultList, maybeRemovedItem)
    }
    (outputList.reverse, removedItem.get)
  }
}
