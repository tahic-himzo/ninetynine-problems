package problems.list

object P18_ExtractSlice {

  //  Extract a slice from index a to index b out a list
  def execute[A](a: Int, b: Int, list: List[A]): List[A] =
    validateInput(a, b, list).fold(throw _, _.slice(a, b))

  def executeAlt[A](a: Int, b: Int, list: List[A]): List[A] =
    validateInput(a, b, list).fold(throw _, _.zipWithIndex.collect {
      case (currentItem, index) if index >= a && index < b => currentItem
    })

  private def validateInput[A](a: Int, b: Int, list: List[A]): Either[Throwable, List[A]] = {
    val listLength = list.length

    (a, b) match {
      case (start, _) if start < 0 =>
        Left(new IllegalArgumentException("Please supply a positive value as starting slice index."))
      case (start, _) if start > listLength =>
        Left(new IllegalArgumentException("Please supply a starting slice index that is <= to the list length."))
      case (start, end) if start > end =>
        Left(new IllegalArgumentException("Please supply a ending slice index that is >= to the starting slice index."))
      case (_, end) if end > listLength =>
        Left(new IllegalArgumentException("Please supply a ending slice index that is <= to the list length."))
      case _ => Right(list)
    }
  }
}
