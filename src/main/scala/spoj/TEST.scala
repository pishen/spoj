package spoj

import scala.io.StdIn

object TEST extends App {
  Iterator.continually(StdIn.readLine().toInt).takeWhile(_ != 42).foreach(println)
}
