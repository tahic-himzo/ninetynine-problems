package problems.list

object P17_SplitList {

  //  Split a list at a given index.
  def execute[A](n: Int, list: List[A]): (List[A], List[A]) = {
    if(n < 0) throw new IllegalArgumentException("Please supply a positive value as splitting index.")
    list.splitAt(n)
  }

  def executeAlt[A](n: Int, list: List[A]): (List[A], List[A]) = {
    if(n < 0) throw new IllegalArgumentException("Please supply a positive value as splitting index.")
    val (left, right) = list.zipWithIndex.foldLeft((List.empty[A], List.empty[A])) {
      case ((left, right), (value, index)) if index < n => (value       :: left, right)
      case ((left, right), (value, _))                  => (left, value :: right)
    }
    (left.reverse, right.reverse)
  }
}
