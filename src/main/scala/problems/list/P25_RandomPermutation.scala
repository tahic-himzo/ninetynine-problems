package problems.list

object P25_RandomPermutation {

  //  Generate a random permutation of the elements of a list.
  def execute[A](list: List[A]): List[A] = P23_SampleAList.execute(list.length, list)

}
