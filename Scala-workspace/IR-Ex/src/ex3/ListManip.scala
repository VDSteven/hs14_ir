package ex3

object ListManip {

  def dist(p1 : (Double,Double), p2 : (Double,Double)) : Double = {
    (p1._1 - p2._1) * (p1._1 - p2._1) + (p1._2 - p2._2) * (p1._2 - p2._2)
  }
  
  def main(args: Array[String]): Unit = {
    val p : List[(Double,Double)] = List((4.3,3.2),(0.5,4.2),(1.2,3.4))
    val l = p.zipWithIndex
    val result = p.zipWithIndex.map{case (p1,i1) => l.filter(_._2 != i1).map{case (p2,i2) => (dist(p1,p2),i2)}.minBy(_._1)._2}
  }

}