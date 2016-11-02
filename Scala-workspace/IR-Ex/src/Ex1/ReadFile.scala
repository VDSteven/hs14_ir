package ex1

object ReadFile {

  def main(args: Array[String]): Unit = {
    //read file to string
    val file : String = io.Source.fromFile("file.txt").mkString;
    //read file to list
    val list : List[String] = io.Source.fromFile("file.txt").getLines.toList;
  }

}