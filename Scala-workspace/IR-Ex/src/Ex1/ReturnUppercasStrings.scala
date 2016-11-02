package ex1

object ReturnUppercasStrings {
  
  def main(args: Array[String]): Unit = {
    val list : List[String] = List("apples","BANANA","batman");
    list.zipWithIndex.filter{case (x,i) => (x == x.toUpperCase)}.map(_._2);
  }

}