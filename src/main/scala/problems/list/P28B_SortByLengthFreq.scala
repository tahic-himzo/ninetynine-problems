package problems.list

object P28B_SortByLengthFreq {

  // The objective is to sort the elements according to their length frequency;
  // i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed first,
  // others with a more frequent length come later.
  def execute[A](list: List[List[A]]): List[List[A]] = {
    val listLengths = list.map(_.length)
    val lengthToFreqMap = listLengths
      .groupBy(x => x)
      .map[Int, Int] {
        case (length, lengthOcc) => (length, lengthOcc.length)
      }

    list.sortWith((a, b) => lengthToFreqMap.apply(a.length) < lengthToFreqMap.apply(b.length))
  }
}
