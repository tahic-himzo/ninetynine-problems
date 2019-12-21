package problems.list

object P24_SampleARange {
  private val random     = scala.util.Random
  private val rangeBegin = 1

  //  Create a list by randomly sampling n items from the range 1..m
  def execute[A](n: Int, rangeEnd: Int): List[Int] = {
    if(n < 0) throw new IllegalArgumentException("Please provide a positive sample size.")
    if(n > Math.abs(rangeEnd - rangeBegin))
      throw new IllegalArgumentException("Please provide a sample size that is <= the range.")
    if(Math.abs(rangeEnd - rangeBegin) <= 1) throw new IllegalArgumentException("Please provide a range span bigger than 1.")
    List
      .unfold(Set.empty[Int], n) {
        case (accumulated, i) if i > 0 =>
          val randomValue = generateRandomDistinct(accumulated, rangeEnd)
          Some((randomValue, (accumulated + randomValue, i - 1)))
        case _ => None
      }
  }

  @scala.annotation.tailrec
  def generateRandomDistinct(acc: Set[Int], rangeEnd: Int): Int = {
    val randomValue =
      if(rangeEnd > rangeBegin) random.between(rangeBegin, rangeEnd)
      else random.between(rangeEnd, rangeBegin)

    if(acc.contains(randomValue)) generateRandomDistinct(acc, rangeEnd)
    else randomValue
  }

}
