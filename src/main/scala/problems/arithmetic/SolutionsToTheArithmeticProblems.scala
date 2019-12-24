package problems.arithmetic

object SolutionsToTheArithmeticProblems {
  implicit class Solutions(value: Int) {

    // Determine whether a given integer number is prime.
    def isPrime: Boolean = value match {
      case i if i < 2 => false
      case _          => LazyList.from(2, step = 1).takeWhile(_ <= Math.sqrt(value.toDouble)).forall(value % _ != 0)
    }

    // Determine the greatest common divisor of two positive integer number using euclids algorithm.
    // Euclids algorithm is based on the principle that the gcd of the smaller number A and the larger number B
    // is the same as the gcd of A and C = B - A. This process can be repeated until A == C ==> C == gcd(A,B)
    def gcd(b: Int): Int = (value, b) match {
      case (a, b) if a <= 0 || b <= 0 => throw new IllegalArgumentException("Please provide positive integerts only")
      case (a, b) if a == b           => a
      case (a, b) if a > b            => b.gcd(a - b)
      case (a, b) if a < b            => a.gcd(b - a)
    }
  }
}