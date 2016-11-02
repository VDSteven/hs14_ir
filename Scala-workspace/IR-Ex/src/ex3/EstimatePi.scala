package ex3

object EstimatePi {

  def sample(i: Int) : Int = {
    val x = Math.random();
    val y = Math.random();
    if ( x*x + y*y < 1) return 1 else 0;
  }
  
  def main(args: Array[String]): Unit = {
    val num_sample = 1000000;
    val count = (1 to num_sample).map(sample).reduce(_ + _);
    val pi : Double = (4.0 * count / num_sample);
    print(pi)
  }

}