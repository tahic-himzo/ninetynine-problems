package problems.arithmetic

import java.util.logging.Logger

import scala.annotation.tailrec
import scala.collection.immutable.LazyList.#::
import scala.concurrent.duration.{DurationDouble, FiniteDuration}

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

    final def primeFactors: List[Int] = {
      @tailrec
      def go(leftOverValue: Int, primesSoFar: List[Int], primesLeft: LazyList[Int]): List[Int] = primesLeft match {
        case x #:: _ if leftOverValue % x == 0 => go(leftOverValue / x, x :: primesSoFar, primesLeft)
        case _ #:: xs => go(leftOverValue, primesSoFar, xs)
        case _        => primesSoFar
      }

      value match {
        case x if x <= 1 => throw new IllegalArgumentException("Please provide positive integers > 1 only")
        case x if x == 2 => List(2)
        case _           => go(value, List.empty[Int], LazyList.range(2, value).filter(_.isPrime)).reverse
      }
    }

    final def primeFactorsMultiplicity: Map[Int, Int] = {
      @tailrec
      def go(leftOverValue: Int, primesSoFar: Map[Int, Int], nextValue: Int): Map[Int, Int] = nextValue match {
        case x if x > value => primesSoFar
        case x if leftOverValue % x == 0 && x.isPrime =>
          go(leftOverValue / x, primesSoFar.updatedWith(x)(maybeValue => Some(maybeValue.fold(1)(_ + 1))), x)
        case x => go(leftOverValue, primesSoFar, x + 1)
      }

      value match {
        case x if x <= 1 => throw new IllegalArgumentException("Please provide positive integers > 1 only")
        case x if x == 2 => Map(2 -> 1)
        case _           => go(value, Map.empty[Int, Int], 2)
      }
    }

    final def totientImproved: Int = value match {
      case v if v <= 0 => throw new IllegalArgumentException("Please provide positive integers only")
      case v if v == 1 => 1
      case _ =>
        primeFactorsMultiplicity.foldLeft(1) {
          case (phiSoFar, (p, m)) => phiSoFar * (p - 1) * Math.pow(p, m - 1).toInt
        }
    }

    final def compareTotients: String = {
      val (_, durationRegular)  = timed(value.totient)
      val (_, durationImproved) = timed(value.totientImproved)

      s"Totient (regular): ${durationRegular.toMillis}ms" + " / " + s"Totient (improved): ${durationImproved.toMillis}ms"
    }

    def listPrimesinRange(end: Int): List[Int] = end match {
      case x if x <= 1 || value <= 1 => throw new IllegalArgumentException("Please provide positive integers > 1 only")
      case x if x < value            => throw new IllegalArgumentException("Please provide an end that is bigger than the beginning.")
      case _                         => Range.inclusive(value, end).filter(_.isPrime).toList
    }

    private def timed[A](f: => A): (A, FiniteDuration) = {
      val start    = System.nanoTime()
      val a        = f
      val end      = System.nanoTime()
      val duration = (end - start).nanoseconds
      (a, duration)
    }
  }
}
