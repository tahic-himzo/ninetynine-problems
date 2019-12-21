package problems.list

object P23_SampleAList {
  private val random = scala.util.Random

  //  Create a list by randomly sampling n items.
  def execute[A](n: Int, list: List[A]): List[A] = {
    if(n < 0) throw new IllegalArgumentException("Please provide a positive sample size.")
    if(n > list.length) throw new IllegalArgumentException("Please provide a sample size that is <= the list length.")
    List
      .unfold(list, n) {
        case (leftOverList, i) if i > 0 =>
          val randomIndex                 = random.nextInt(leftOverList.length)
          val (listWithItemRemoved, item) = P20_RemoveKth.execute(randomIndex, leftOverList)
          Some(item, (listWithItemRemoved, i - 1))
        case _ => None
      }
  }

}
