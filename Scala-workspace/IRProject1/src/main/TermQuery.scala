package main

import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.processing.Tokenizer

class TermQuery(query: String) {

  //Term-based model score, using logarithmic term frequency
  
  val qterms = Tokenizer.tokenize(query).distinct
  val length = qterms.length
    
  
  def score(logtfs: Map[String,Double], idf: Map[String,Double]): Double = {
    qterms.map(q => logtfs.getOrElse(q, 0.0) * idf.getOrElse(q, 0.0)).sum
  }
}