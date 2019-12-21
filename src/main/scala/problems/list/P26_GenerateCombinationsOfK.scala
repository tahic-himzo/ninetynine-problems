package problems.list

object P26_GenerateCombinationsOfK {

  //  Generate the combinations of K distinct objects chosen from the N elements of a list.
  def execute[A](k: Int, list: List[A]): List[List[A]] = {
    def go(lockedIn: List[A], leftOverList: List[A], leftOverK: Int): List[List[A]] =
      List
        .unfold(Option(leftOverList)) {
          case Some(x :: xs) if leftOverK > 0 => Some((go(x :: lockedIn, xs, leftOverK - 1), Some(xs)))
          case Some(_) if leftOverK == 0 => Some((List(lockedIn), None))
          case _                         => None
        }
        .flatten

    list.length match {
      case len if k > len => throw new IllegalArgumentException("Please provide a k that is >= the list length.")
      case _ if k < 0     => throw new IllegalArgumentException("Please provide a positive k.")
      case _              => go(List.empty, list, k).map(_.reverse)
    }
  }
}
