package problems.arithmetic

import scala.annotation.tailrec

object SolutionsToTheArithmeticProblems {
  implicit class Solutions(value: Int) {

    // Determine whether a given integer number is prime.
    final def isPrime: Boolean = value match {
      case i if i < 2 => false
      case _          => LazyList.from(2, step = 1).takeWhile(_ <= Math.sqrt(value.toDouble)).forall(value % _ != 0)
    }

    // Determine the greatest common divisor of two positive integer number using euclids algorithm.
    // Euclids algorithm is based on the principle that the gcd of the smaller number A and the larger number B
    // is the same as the gcd of A and C = B - A. This process can be repeated until A == C ==> C == gcd(A,B)
    @tailrec
    final def gcd(b: Int): Int = (value, b) match {
      case (a, b) if a <= 0 || b <= 0 => throw new IllegalArgumentException("Please provide positive integers only")
      case (a, b) if a == b           => a
      case (a, b) if a > b            => b.gcd(a - b)
      case (a, b) if a < b            => a.gcd(b - a)
    }

    final def isCoprimeTo(b: Int): Boolean = gcd(b) == 1

    final def totient: Int = value match {
      case v if v <= 0 => throw new IllegalArgumentException("Please provide positive integers only")
      case v if v == 1 => 1
      case v =>
        LazyList.range(1, v).foldLeft(0) {
          case (totientSoFar, x) if isCoprimeTo(x) => totientSoFar + 1
          case (totientSoFar, _)                   => totientSoFar
        }
    }
  }
}
