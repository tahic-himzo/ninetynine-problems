package problems.list

object P07_FlattenList {

  //  Flatten a nested list structure
  def execute(list: List[Any]): List[Any] = flatten(list).reverse

  private def flatten(list: List[Any]): List[Any] = list.foldLeft(List.empty[Any]) {
    case (acc, xs: List[Any]) => flatten(xs) ::: acc
    case (acc, x) => x :: acc
  }
}
