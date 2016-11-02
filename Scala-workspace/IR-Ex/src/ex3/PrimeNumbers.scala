package ex3

object PrimeNumbers {

  def main(args: Array[String]): Unit = {
    val l = (2 to 100)
    val result = l.filter(x => l.filter(y => y < x && x % y == 0).isEmpty)
  }
  
}