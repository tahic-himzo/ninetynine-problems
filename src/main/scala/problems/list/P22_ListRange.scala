package problems.list

object P22_ListRange {

  //  Create a list with the items between start and end.
  def execute[A](start: Int, end: Int): List[Int] = end - start match {
    case diff if diff >= 0 =>
      List.unfold(start) {
        case value if value <= end => Some((value, value + 1))
        case _                     => None
      }
    case diff if diff < 0 =>
      List.unfold(start) {
        case value if value >= end => Some((value, value - 1))
        case _                     => None
      }
  }

}
