package problems.logic {

  object LogicProblems {

    def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
      case (true, true)   => true
      case (true, false)  => false
      case (false, true)  => false
      case (false, false) => false
    }

    def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
      case (true, true)   => true
      case (true, false)  => true
      case (false, true)  => true
      case (false, false) => false
    }

    def not(a: Boolean): Boolean = a match {
      case true  => false
      case false => true
    }

    def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

    def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

    def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))

    def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

    def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))

    def table2(f: (Boolean, Boolean) => Boolean): Unit = println(createTruthTable(f))

    private[logic] def createTruthTable(f: (Boolean, Boolean) => Boolean): String = {
      val input = List((true, true), (true, false), (false, true), (false, false))
      val results = input
        .foldLeft(List("A     B     result")) {
          case (output, (a, b)) =>
            def paddedBooleanString(x: Boolean): String = x.toString.padTo(5, ' ')

            s"${paddedBooleanString(a)} ${paddedBooleanString(b)} ${f(a, b)}".stripMargin :: output
        }
        .reverse
      results.mkString("\n")
    }
  }
}
