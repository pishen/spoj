package spoj

import scala.io.StdIn

object DIVSUM extends App {
  val num = StdIn.readLine().toInt
  (1 to num).foreach { _ =>
    val input = StdIn.readLine().toInt
    if (input == 1) {
      println(0)
    } else {
      def divide(fragments: Seq[Int], n: Int): Seq[Int] = {
        val a = (2 to math.sqrt(n).ceil.toInt).find(n % _ == 0).getOrElse(n)
        val b = n / a
        if (b == 1) a +: fragments else divide(a +: fragments, b)
      }

      val fragments = divide(Seq.empty, input)
      val res = (1 until fragments.size).flatMap(s => fragments.combinations(s).map(_.product)).sum + 1
      println(res)
    }
  }
}
