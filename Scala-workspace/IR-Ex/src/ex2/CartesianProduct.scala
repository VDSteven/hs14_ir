package ex2

object CartesianProduct {

  def main(args: Array[String]): Unit = {
    val l1 : List[Int] = List(1,2,3,4,5);
    val l2 : List[Int] = List(5,4,3,2,1);
    //yield
    val yield_product = for (x <- l1; y <- l2) yield (x,y);
    //flatMap/map
    val map_product = l1.flatMap(x => l2.map(y => (x,y)));
  }

}