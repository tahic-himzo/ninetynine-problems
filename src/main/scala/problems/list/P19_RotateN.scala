package problems.list

object P19_RotateN {

  //  Rotate a list by n
  def execute[A](n: Int, list: List[A]): List[A] = list.length match {
    case 0 => list
    case len if n >= 0 =>
      val firstPartOfList  = list.drop(n % len)
      val secondPartOfList = list.take(n % len)
      firstPartOfList ::: secondPartOfList
    case len =>
      val firstPartOfList  = list.takeRight(Math.abs(n) % len)
      val secondPartOfList = list.dropRight(Math.abs(n) % len)
      firstPartOfList ::: secondPartOfList
  }
}
