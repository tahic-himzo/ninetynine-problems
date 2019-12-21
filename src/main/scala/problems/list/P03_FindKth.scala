package problems.list

object P03_FindKth {

  //  Find the kth element of a list.
  def execute[A](index: Int, list: List[A]): A =
    list.zipWithIndex.find { case (_, i) => i == index }
      .map(_._1)
      .getOrElse(throw new IllegalArgumentException(s"Please provide a least with at least ${index + 1} items"))
}
