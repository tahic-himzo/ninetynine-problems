package problems.arithmetic

import scala.annotation.tailrec
import scala.collection.immutable.LazyList.#::
import scala.concurrent.duration.{DurationDouble, FiniteDuration}

object ArithmeticProblems {
  implicit class S99Int(value: Int) {

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

    // Determine whether two positive integer numbers are coprime.
    final def isCoprimeTo(b: Int): Boolean = gcd(b) == 1

    // Calculate Euler's totient function
    final def totient: Int = value match {
      case v if v <= 0 => throw new IllegalArgumentException("Please provide positive integers only")
      case v if v == 1 => 1
      case v =>
        LazyList.range(1, v).foldLeft(0) {
          case (totientSoFar, x) if isCoprimeTo(x) => totientSoFar + 1
          case (totientSoFar, _)                   => totientSoFar
        }
    }

    // Determine the prime factors of a given positive integer.
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

    // Determine the prime factors and their multiplicity of a given positive integer.
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

    // Calculate Euler's totient function by using the prime factors.
    final def totientImproved: Int = value match {
      case v if v <= 0 => throw new IllegalArgumentException("Please provide positive integers only")
      case v if v == 1 => 1
      case _ =>
        primeFactorsMultiplicity.foldLeft(1) {
          case (phiSoFar, (p, m)) => phiSoFar * (p - 1) * Math.pow(p, m - 1).toInt
        }
    }

    // Use the solutions of problems P34 and P37 to compare the algorithms. Try to calculate phi(10090) as an example.
    final def compareTotients: String = {
      val (_, durationRegular)  = timed(value.totient)
      val (_, durationImproved) = timed(value.totientImproved)

      s"Totient (regular): ${durationRegular.toMillis}ms" + " / " + s"Totient (improved): ${durationImproved.toMillis}ms"
    }

    // Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
    def listPrimesinRange(end: Int): List[Int] = end match {
      case x if x <= 1 || value <= 1 => throw new IllegalArgumentException("Please provide positive integers > 1 only")
      case x if x < value            => throw new IllegalArgumentException("Please provide an end that is bigger than the beginning.")
      case _                         => Range.inclusive(value, end).filter(_.isPrime).toList
    }

    // Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
    // Write a function to find the two prime numbers that sum up to a given even integer.
    def goldbach: (Int, Int) = value match {
      case v if v <= 2 => throw new IllegalArgumentException("Please provide positive integers > 2 only")
      case v if v % 2 != 0 => throw new IllegalArgumentException("Please provide even integers only")
      case _ =>
        val primes        = 2.listPrimesinRange(value)
        val primesReverse = primes.reverse
        primes
          .flatMap(
            p =>
              primesReverse.dropWhile(_ + p > value).headOption match {
                case Some(counterPart) if counterPart + p == value => Some((p, counterPart))
                case _                                             => None
            }
          )
          .head
    }

    //Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
    def printGoldbachList(end: Int): Unit = end match {
      case x if x <= value => throw new IllegalArgumentException("Please specify a range where the end is bigger than the start.")
      case _ if value <= 2 => throw new IllegalArgumentException("Please specify a start that is bigger than 2.")
      case _ =>
        val range = Range.inclusive(value, end).filter(_ % 2 == 0)
        val goldbachs = range.foldLeft(List.empty[String]) {
          case (goldbachStrings, a) =>
            val (primeA, primeB) = a.goldbach
            val goldbachString   = s"$a = $primeA + $primeB"
            goldbachString :: goldbachStrings
        }
        println(goldbachs.mkString("\n"))
    }

    // In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
    // Very rarely, the primes are both bigger than, say, 50.
    // Try to find out how many such cases there are in a range.
    def printGoldbachListLimited(end: Int, minimumPrimeSize: Int): Unit = end match {
      case x if x <= value            => throw new IllegalArgumentException("Please specify a range where the end is bigger than the start.")
      case _ if value <= 2            => throw new IllegalArgumentException("Please specify a start that is bigger than 2.")
      case _ if minimumPrimeSize <= 0 => throw new IllegalArgumentException("Please specify a non-negative minimumPrimeSize.")
      case _ =>
        val range = Range.inclusive(value, end).filter(_ % 2 == 0)
        val goldbachs = range.foldLeft(List.empty[String]) {
          case (goldbachStrings, a) =>
            val (primeA, primeB) = a.goldbach
            val goldbachString   = s"$a = $primeA + $primeB"

            if(primeA > minimumPrimeSize && primeB > minimumPrimeSize) goldbachString :: goldbachStrings
            else goldbachStrings
        }
        println(goldbachs.mkString("\n"))
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
