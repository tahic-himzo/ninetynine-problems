package problems.list

object P27B_GroupDisjoint {

  //  P27A, but generalized
  def execute[A](grouping: List[Int], list: List[A]): List[List[List[A]]] =
    (list.length, grouping.sum) match {
      case (listLen, gSum) if gSum != listLen =>
        throw new IllegalArgumentException("Please provide a grouping whose sum matches the amount of items")
      case _ =>
        grouping.foldLeft(List.empty: List[List[List[A]]]) {
          case (Nil, groupSize) =>
            P26_GenerateCombinationsOfK.execute(groupSize, list).map(List(_))
          case (res, groupSize) =>
            res.flatMap(lockedIn =>
              P26_GenerateCombinationsOfK.execute(groupSize, list.diff(lockedIn.flatten)).map(_ :: lockedIn))
        }
    }
}
