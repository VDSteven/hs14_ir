package vsteven.irproject2

import java.io._

import ch.ethz.dal.classifier.processing.{ReutersRCVParse, Document, ReutersCorpusIterator}
import com.github.aztek.porterstemmer.PorterStemmer

/**
 * Created by Steven on 19/11/2014.
 */
class NaiveBayes(t: List[String]) {

  def pre_process(tokens: List[String]):List[String] = {
    val StopWords = Set("a","able","about","across","after","all","almost","also","am","among","an","and","any","are","as","at","be","because","been","but","by","can","cannot","could","dear","did","do","does","either","else","ever","every","for","from","get","got","had","has","have","he","her","hers","him","his","how","however","i","if","in","into","is","it","its","just","least","let","like","likely","may","me","might","most","must","my","neither","no","nor","not","of","off","often","on","only","or","other","our","own","rather","said","say","says","she","should","since","so","some","than","that","the","their","them","then","there","these","they","this","tis","to","too","twas","us","wants","was","we","were","what","when","where","which","while","who","whom","why","will","with","would","yet","you","your");
    val result = tokens.filter(StopWords)
    return result.map(t => PorterStemmer.stem(t))
  }

  val topics = t
  val p_c = scala.collection.mutable.Map[String,Double]()
  val p_wc_n = scala.collection.mutable.Map[String,Map[String,Double]]()
  val p_wc_d = scala.collection.mutable.Map[String,Double]()

  def train(docs:ReutersCorpusIterator) = {
    println("train start")
    //slide 17
    val topicCounts = scala.collection.mutable.Map[String, Int]()
    var count = 0;
    var vocabulary = List[String]()
    val topicDocs = scala.collection.mutable.Map[String, List[String]]()
    while (docs.hasNext) {
      val doc = docs.next
      val tokens = pre_process(doc.tokens)
      topicCounts ++= doc.topics.map(c => (c -> (1 + topicCounts.getOrElse(c, 0))))
      count += 1
      vocabulary = (vocabulary ++ tokens.distinct).distinct
      p_wc_d ++= doc.topics.map(c => (c -> (p_wc_d.getOrElse(c,0.0) + tokens.length.toDouble)))
      val tf:Map[String,Double] = tokens.groupBy(identity).mapValues(l => l.length.toDouble)
      p_wc_n ++= doc.topics.map(c => (c -> (tf ++ p_wc_n.getOrElse(c,Map[String,Double]()).map{case (k,v) => k -> (v + tf.getOrElse(k,0.0))})))
    }

    for (t <- topics) {
      val cat = t.split("\t").head
      //p_c code from slide 21
      val P_cat = topicCounts.getOrElse(cat,0) / count.toDouble
      p_c += (cat -> P_cat)

      //p_wc code from slide 22
      val vocabSize = vocabulary.length
      val tks = topicDocs.getOrElse(cat,List[String]())
      p_wc_d += (cat ->  (p_wc_d.getOrElse(cat,0.0) + vocabSize))
    }
  }

  def labelled_classify(docs:ReutersCorpusIterator) = {
    println("label start")
    //slide 18
    val assigned_topics = scala.collection.mutable.Map[String,Set[String]]()
    val real_topics = scala.collection.mutable.Map[String,Set[String]]()
    while(docs.hasNext) {
      val doc = docs.next
      val tokens = pre_process(doc.tokens)
      val estimate = scala.collection.mutable.Map[String,Double]()
      for (t <- topics) {
        val cat = t.split("\t").head
        val log_p_c = Math.log(p_c.getOrElse(cat,1.0))
        var sum = 0.0 //sum of tf(w,d)*log(p_wc(w,c))
        tokens.distinct.foreach(w => sum += tokens.count(_ == w) * Math.log((p_wc_n.getOrElse(cat,Map()).getOrElse(w,0.0)+1.0)/p_wc_d.getOrElse(cat,1.0)))
        estimate += (cat -> (log_p_c + sum))
      }
      //choose 4 best results
      val topic_set = estimate.toList.sortWith(_._2 > _._2).take(4).map(l => l._1).toSet
      assigned_topics += (doc.name -> topic_set)
      real_topics += (doc.name -> doc.topics)
    }

    val writer = new PrintWriter(new File("classify-steven-vandamme-l-nb.run"))
    writer.write(PrecisionRecallF1.evaluation(assigned_topics,real_topics) + "\n")
    assigned_topics.foreach(a => writer.write(a._1 + " " + a._2.toString().replace("Set","").replace("(","").replace(")","").replace(",","") + "\n"))
    writer.close()
  }

  def test_classify(docs:ReutersCorpusIterator) = {
    println("test start")
    //slide 18
    val assigned_topics = scala.collection.mutable.Map[String, Set[String]]()
    while (docs.hasNext) {
      val doc = docs.next
      val tokens = pre_process(doc.tokens)
      val estimate = scala.collection.mutable.Map[String, Double]()
      for (t <- topics) {
        val cat = t.split("\t").head
        val log_p_c = Math.log(p_c.getOrElse(cat, 1.0))
        var sum = 0.0 //sum of tf(w,d)*log(p_wc(w,c))
        tokens.distinct.foreach(w => sum += tokens.count(_ == w) * Math.log(p_wc_n.getOrElse(cat, Map()).getOrElse(w, 1.0)))
        estimate += (cat -> (log_p_c + sum))
      }
      //choose 4 best results
      val topic_set = estimate.toList.sortWith(_._2 > _._2).take(4).map(l => l._1).toSet
      assigned_topics += (doc.name -> topic_set)
    }

    val writer = new PrintWriter(new File("classify-steven-vandamme-u-nb.run"))
    assigned_topics.foreach(a => writer.write(a._1 + " " + a._2.toString().replace("Set","").replace("(","").replace(")","").replace(",","") + "\n"))
    writer.close()
  }

}
