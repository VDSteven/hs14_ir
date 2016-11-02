package ex2

import scala.util.Try

object ReturnValidInt {

  def main(args: Array[String]): Unit = {
    val l : List[String] = List("1","apple","22","hello");
    l.map(ele => Try(ele.toInt).toOption)
  }

}