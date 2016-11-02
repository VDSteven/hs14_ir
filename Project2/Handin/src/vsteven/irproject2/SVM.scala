package vsteven.irproject2

import java.io.{File, PrintWriter}

import ch.ethz.dal.classifier.processing.ReutersCorpusIterator
import com.github.aztek.porterstemmer.PorterStemmer

/**
 * Created by Steven on 22/11/2014.
 */
class SVM(t: List[String]) {

  def pre_process(tokens: List[String]):List[String] = {
    val StopWords = Set("a","able","about","across","after","all","almost","also","am","among","an","and","any","are","as","at","be","because","been","but","by","can","cannot","could","dear","did","do","does","either","else","ever","every","for","from","get","got","had","has","have","he","her","hers","him","his","how","however","i","if","in","into","is","it","its","just","least","let","like","likely","may","me","might","most","must","my","neither","no","nor","not","of","off","often","on","only","or","other","our","own","rather","said","say","says","she","should","since","so","some","than","that","the","their","them","then","there","these","they","this","tis","to","too","twas","us","wants","was","we","were","what","when","where","which","while","who","whom","why","will","with","would","yet","you","your");
    val result = tokens.filter(StopWords)
    return result.map(t => PorterStemmer.stem(t))
  }

  def logistic(th:Map[String,Double],x:Map[String,Double]): Double = {
    th.map{case (k,v) => v*x.getOrElse(k,0.0)}.sum
  }

  val topics = t
  val theta = scala.collection.mutable.Map[String,Map[String,Double]]()
  var step = 1
  val lambda = 0.2

  def train(docs:ReutersCorpusIterator) = {
    println("train start")
    //theta start empty map
    //update slide 29
    while (docs.hasNext) {
      val doc = docs.next
      val tokens = pre_process(doc.tokens)
      val x = tokens.groupBy(identity).mapValues(l => l.length.toDouble)
      for (t <- topics) {
        val cat = t.split("\t").head
        //if else to see if y = 1 or y = -1
        if (doc.topics.contains(cat)) {
          val thetashrink:Map[String,Double] = theta.getOrElse(cat,Map[String,Double]()).mapValues[Double](v => v * (1-1.0/step))
          val margin = 1.0 - (x.map{case (k,v) => v*theta.getOrElse(cat,Map[String,Double]()).getOrElse(k,0.0)}.sum)
          if (margin <= 0) {
            theta += (cat -> thetashrink)
          }
          else {
            theta += (cat -> (thetashrink ++ x.mapValues[Double](v => v * (1.0/(lambda * step))).map{case (k,v) => k -> (v + thetashrink.getOrElse(k,0.0))}))
          }
        }
        else {
          val thetashrink:Map[String,Double] = theta.getOrElse(cat,Map[String,Double]()).mapValues[Double](v => v * (1-1.0/step))
          val margin = 1.0 - (-1.0 * x.map{case (k,v) => v*theta.getOrElse(cat,Map[String,Double]()).getOrElse(k,0.0)}.sum)
          if (margin <= 0) {
            theta += (cat -> thetashrink)
          }
          else {
            theta += (cat -> (thetashrink ++ x.mapValues[Double](v => v * (1.0/(lambda * step)) * -1.0).map{case (k,v) => k -> (v + thetashrink.getOrElse(k,0.0))}))
          }
        }
      }
      step += 1
    }
  }

  def labelled_classify(docs:ReutersCorpusIterator) = {
    //use trained theta
    //1. formula slide 26 or g() slide 27
    //precision recall fa
    println("label start")
    val assigned_topics = scala.collection.mutable.Map[String,Set[String]]()
    val real_topics = scala.collection.mutable.Map[String,Set[String]]()
    while(docs.hasNext) {
      val doc = docs.next
      val tokens = pre_process(doc.tokens)
      val x = tokens.groupBy(identity).mapValues(l => l.length.toDouble)
      val estimate = scala.collection.mutable.Map[String,Double]()
      for (t <- topics) {
        val cat = t.split("\t").head
        estimate += (cat -> logistic(theta.getOrElse(cat,Map[String,Double]()),x))
      }
      //take top 3 estimates
      val topic_set = estimate.toList.sortWith(_._2 > _._2).take(3).map(l => l._1).toSet
      assigned_topics += (doc.name -> topic_set)
      real_topics += (doc.name -> doc.topics)
    }

    val writer = new PrintWriter(new File("classify-steven-vandamme-l-svm.run"))
    writer.write(PrecisionRecallF1.evaluation(assigned_topics,real_topics) + "\n")
    assigned_topics.foreach(a => writer.write(a._1 + " " + a._2.toString().replace("Set","").replace("(","").replace(")","").replace(",","") + "\n"))
  }

  def test_classify(docs:ReutersCorpusIterator) = {
    //use trained theta
    //1. formula slide 26 or g() slide 27
    println("test start")
    val assigned_topics = scala.collection.mutable.Map[String,Set[String]]()
    while(docs.hasNext) {
      val doc = docs.next
      val tokens = pre_process(doc.tokens)
      val x = tokens.groupBy(identity).mapValues(l => l.length.toDouble)
      val estimate = scala.collection.mutable.Map[String,Double]()
      for (t <- topics) {
        val cat = t.split("\t").head
        estimate += (cat -> logistic(theta.getOrElse(cat,Map[String,Double]()),x))
      }
      //take top 3 estimates
      val topic_set = estimate.toList.sortWith(_._2 > _._2).take(3).map(l => l._1).toSet
      assigned_topics += (doc.name -> topic_set)
    }

    val writer = new PrintWriter(new File("classify-steven-vandamme-u-svm.run"))
    assigned_topics.foreach(a => writer.write(a._1 + " " + a._2.toString().replace("Set","").replace("(","").replace(")","").replace(",","") + "\n"))
    writer.close()
  }

}
