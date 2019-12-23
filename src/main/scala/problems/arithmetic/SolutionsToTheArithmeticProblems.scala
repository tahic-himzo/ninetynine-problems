package problems.arithmetic

object SolutionsToTheArithmeticProblems {
  implicit class Solutions(value: Int) {

    // Determine whether a given integer number is prime.
    def isPrime: Boolean = value match {
      case i if i < 2 => false
      case _          => LazyList.from(2, step = 1).takeWhile(_ <= Math.sqrt(value.toDouble)).forall(value % _ != 0)
    }

    // Determine the greatest common divisor of two positive integer numbers.
    def gcd(a: Int, b: Int): Int =
      LazyList.from(2, step = 1).takeWhile(_ <= a).find(div => a % div == 0 && b % div == 0)
  }
}
