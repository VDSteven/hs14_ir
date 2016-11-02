package vsteven.irproject2

/**
 * Created by Steven on 19/11/2014.
 */

import ch.ethz.dal.classifier.processing.ReutersCorpusIterator
import com.github.aztek.porterstemmer.PorterStemmer
import scala.io.Source

class Main {


}

object Main {

  def get_topics(path:String):List[String] = {
    return Source.fromFile(path).getLines().toList.filter(!_.startsWith(";"))
  }

  def main(args : Array[String]) = {
    val topics = get_topics("D:/Projects/Scala/Assignment2/topic_codes.txt")
    val train_path = "D:/Projects/Scala/Assignment2/data/training/train"
    val labelled_path = "D:/Projects/Scala/Assignment2/data/test-with-labels/test-with-labels"
    val test_path = "D:/Projects/Scala/Assignment2/data/test-without-labels/test-without-labels"
    val train_iter = new ReutersCorpusIterator(train_path)
    val labelled_iter = new ReutersCorpusIterator(labelled_path)
    val test_iter = new ReutersCorpusIterator(test_path)
    val input = args.head.toInt
    //0 = NaiveBayes - 1 = LogisticReg - 2 = SVM
    if (input == 0) {
      val classifier = new NaiveBayes(topics)
      classifier.train(train_iter)
      classifier.labelled_classify(labelled_iter)
      classifier.test_classify(test_iter)
    }
    else if (input == 1) {
      val classifier = new LogisticRegression(topics)
      classifier.train(train_iter)
      classifier.labelled_classify(labelled_iter)
      classifier.test_classify(test_iter)
    }
    else if (input == 2) {
      val classifier = new SVM(topics)
      classifier.train(train_iter)
      classifier.labelled_classify(labelled_iter)
      classifier.test_classify(test_iter)
    }
    else {
      println("Something went wrong! (Not correct input)")
    }
  }
}