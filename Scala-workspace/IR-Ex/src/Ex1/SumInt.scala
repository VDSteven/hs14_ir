package ex1

object SumInt {

  def main(args: Array[String]): Unit = {
    //reduce
    val result = (1 to 100).reduce(_ + _);
    //foreach
    var sum = 0;
    val result2 = (1 to 100).foreach(sum += _);
  }

}