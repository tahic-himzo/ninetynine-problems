package problems.list

object P16_DropEveryN {

  //  Drop every n-th item in a list.
  def execute[A](n: Int, list: List[A]): List[A] = {
    if(n <= 0) throw new IllegalArgumentException("Please supply a non-zero, positive value as dropping interval")
    list.zipWithIndex.filterNot {
      case (_, index) => index % n == (n - 1)
    }.map(_._1)
  }
}
