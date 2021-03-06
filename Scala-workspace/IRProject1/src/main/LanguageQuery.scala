package main

import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.processing.Tokenizer

class LanguageQuery(query: String) {
  
  val qterms = Tokenizer.tokenize(query).distinct
  val length = qterms.length
  
  def log2(x: Double) = scala.math.log10(x) / scala.math.log10(2.0)
   
  def score(maxs: Map[String,Double], smooths: Map[String,Double]): Double = {
    qterms.map(q => maxs.getOrElse(q, 0.0) + smooths.getOrElse(q, 1.0)).product
  }

}