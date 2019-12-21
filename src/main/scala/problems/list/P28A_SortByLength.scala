package problems.list

object P28A_SortByLength {

  //   We suppose that a list contains elements that are lists themselves.
  //   The objective is to sort the elements of the list according to their length.
  //   E.g. short lists first, longer lists later, or vice versa.
  def execute[A](list: List[List[A]]): List[List[A]] = list.sortWith((a, b) => a.length > b.length)
}
