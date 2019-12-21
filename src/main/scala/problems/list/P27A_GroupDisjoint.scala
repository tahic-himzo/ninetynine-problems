package problems.list

object P27A_GroupDisjoint {

  //  In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
  //  Write a function that generates all the possibilities.

  def execute[A](list: List[A]): List[List[List[A]]] =
    list.length match {
      case len if len != 9 => throw new IllegalArgumentException("Please provide a list with a length of 9.")
      case _ =>
        P26_GenerateCombinationsOfK
          .execute(2, list)
          .flatMap(
            firstGroup => {
              val secondGroups: List[List[A]]                    = P26_GenerateCombinationsOfK.execute(3, list.diff(firstGroup))
              val firstAndSecondGroups: List[(List[A], List[A])] = secondGroups.map((firstGroup, _))
              firstAndSecondGroups.flatMap {
                case (a, b) =>
                  val thirdGroups: List[List[A]] = P26_GenerateCombinationsOfK.execute(4, list.diff(a ::: b))
                  thirdGroups.map(List(a, b, _))
              }
            }
          )
    }
}
