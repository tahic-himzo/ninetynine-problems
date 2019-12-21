package problems.list

object P02_Penultimate {

  //  Find the last but one element of a list.
  @scala.annotation.tailrec
  def execute[A](list: List[A]): A =
    list match {
      case _ :: Nil => throw new IllegalArgumentException("Please provide a list with at least two elements")
      case a :: _ :: Nil => a
      case _ :: xs => execute(xs)
    }
}
