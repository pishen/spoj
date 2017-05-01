package spoj

import scala.io.StdIn

object ADFRUITS extends App {
  Iterator.continually(StdIn.readLine()).takeWhile(_ != null).foreach { line =>
    val a = "_" + line.split(" ")(0)
    val b = "_" + line.split(" ")(1)

    lazy val table: Stream[Stream[Int]] = Stream.tabulate(a.size, b.size){ (i, j) =>
      val leftTop = if (i > 0 && j > 0) {
        table(i - 1)(j - 1) + (if (a(i) == b(j)) 1 else 0)
      } else 0
      
      val left = if (j > 0) table(i)(j - 1) else 0
      val top = if (i > 0) table(i - 1)(j) else 0
      
      leftTop max left max top
    }
    
    def trace(str: String, i: Int, j: Int): String = {
      if (i > 0 && j > 0 && table(i - 1)(j - 1) + (if (a(i) == b(j)) 1 else 0) == table(i)(j)) {
        if (a(i) == b(j)) {
          trace(a(i).toString + str, i - 1, j - 1)
        } else {
          trace(a(i).toString + b(j).toString + str, i - 1, j - 1)
        }
      } else if (j > 0 && table(i)(j - 1) == table(i)(j)) {
        trace(b(j).toString + str, i, j - 1)
      } else if (i > 0 && table(i - 1)(j) == table(i)(j)) {
        trace(a(i).toString + str, i - 1, j)
      } else {
        str
      }
    }
    
    println(trace("", a.size - 1, b.size - 1))
  }
}
