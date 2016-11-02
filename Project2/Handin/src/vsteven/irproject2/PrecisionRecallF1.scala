package vsteven.irproject2

import java.io.{File, PrintWriter}

/**
 * Created by Steven on 24/11/2014.
 */
class PrecisionRecallF1 {

}

object PrecisionRecallF1 {

  def evaluation(assigned:scala.collection.mutable.Map[String,Set[String]],truth:scala.collection.mutable.Map[String,Set[String]]):String = {
    var count = 0
    var set_precision = 0.0
    var set_recall = 0.0
    for ((id,a_topics) <- assigned) {
      count += 1
      val t_topics = truth.getOrElse(id,Set())
      val truePos = (a_topics & t_topics).size.toDouble
      var precision = 0.0
      if (a_topics.size == 0) {
        precision = truePos
      }
      else {
        precision = truePos / a_topics.size
      }
      var recall = 0.0
      if (t_topics.size == 0) {
        recall = truePos
      }
      else {
        recall = truePos / t_topics.size
      }
      set_precision += precision
      set_recall += recall
    }
    set_precision = set_precision / count
    set_recall = set_recall / count
    val set_f1 = 2 * (set_precision * set_recall) / (set_precision + set_recall)
    return (set_precision + " " + set_recall + " " + set_f1)
  }

}
