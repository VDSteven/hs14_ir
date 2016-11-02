package ex2

object ListSize {
  
  def main(args: Array[String]): Unit = {
    val l : List[List[String]] = Nil;
    //yield
    val yield_len = for (list <- l; s <- list; if s.length > 0) yield s.length;
    //flatMap
    val map_len = l.flatMap(list => list.filter(s => s.length > 0).map(s => s.length));
  }

}