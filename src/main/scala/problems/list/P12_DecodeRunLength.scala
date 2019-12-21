package problems.list

object P12_DecodeRunLength {

  //  Decode a run-length encoded list
  def execute[A](list: List[(Int, A)]): List[A] =
    list.foldLeft(List.empty[A]) {
      case (decoded, (count, value)) => List.fill(count)(value) ::: decoded
    }.reverse
}
