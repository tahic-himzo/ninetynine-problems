package problems.list

object P01_LastElement {

  // Find the last element of a list.
  def execute[A](list: List[A]): A =
    list.lastOption.getOrElse(throw new IllegalArgumentException("Please provide a non-empty list."))
}
