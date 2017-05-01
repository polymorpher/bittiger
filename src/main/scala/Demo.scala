import java.io.File

import scala.io.Source

/**
  * Created by Aaron Li (aaron@potatos.io) on 5/1/17.
  */

// Bag of words demo
object BOWDemo {

  // utility for printing topics and word probabilities
  // adding an implicit method to type Array[Map[Int,Double]]
  implicit class PrinterUtil(phi: Array[Map[Int, Double]]) {
    def print(dict: Dict, tp: Array[Double]): Unit = println(
      tp.zipWithIndex.zip(phi).sortBy(-_._1._1).map { case ((pk, k), m) =>
        s"Topic $k, prob: $pk: " + m.toSeq.sortBy(-_._2).take(10).map { case (w, pw) => s"[${dict.lookup(w)}, ${pw.formatted("%.3f")}]" }.mkString(", ")
      }.mkString("\n"))
  }

  // program entry point
  def main(args: Array[String]): Unit = {
    val vf = s"/bow/vocab.${args(0)}.txt"
    val df = s"/bow/docword.${args(0)}.txt"
    val dict = new Dict()
    val numTopics = 128
    // read vocabulary file and build dictionary
    Source.fromInputStream(getClass.getResourceAsStream(vf)).getLines().zipWithIndex.foreach { case (w, i) => dict.add(i + 1, w) }

    // read document data file and convert to 2D int array
    val docs = Source.fromInputStream(getClass.getResourceAsStream(df)).getLines().drop(3).toSeq.groupBy(e => e.split(" ")(0)).map(_._2.map(line => {
      val parts = line.split(" ")
      parts(1).toInt -> parts(2).toInt
    }).toMap).map(tokenMap => {
      tokenMap.flatMap { case (k, v) => Array.fill(v)(k) }.toSeq
    }).toSeq.take(1500)
    println(s"num docs = ${docs.length} avg doc length = ${docs.map(_.length).sum.toDouble / docs.length} num vocab = ${dict.lookup.size} num topics = $numTopics")

    // initialise LDA instance, with hyperparameters and document data
    var lda: LDA = null
    if (args.length > 1 && args(1) == "lda") {
      lda = new LDA(docs, numTopics, dict.lookup.size, 0.1, 0.1)
    } else {
      lda = new SparseLDA(docs, numTopics, dict.lookup.size, 0.1, 0.1)
    }

    // run Gibbs sampling and print results for each iteration
    0 until 100 foreach { i =>
      println(s"Iteration $i")
      val t = System.nanoTime()
      lda.run(1)
      val t2 = System.nanoTime()
      println(s"Iteration $i done")
      lda.computePhi().print(dict, lda.getTopicProbs())
      println(s"time = ${(t2 - t).toDouble / 1e+9} seconds")
    }
  }
}

object TextDemo {

  import BOWDemo.PrinterUtil

  def main(args: Array[String]): Unit = {
    val sr = new SNAPReader
    val texts = sr.read(new File(getClass.getResource("/text/SanDiskUltra64GB.txt").toURI)).map(_.reviewText)
    val sdocs = texts.map(t => NLPCore.parse(t).flatMap { case (k, v) => Array.fill(v)(k) }.toSeq)
    val dict = new Dict()
    val docs = sdocs.map(_.map(dict.add))
    val numTopics = 128
    println(s"num docs = ${docs.length} avg doc length = ${docs.map(_.length).sum.toDouble / docs.length} num vocab = ${dict.lookup.size} num topics = $numTopics")
    var lda: LDA = null
    if (args.length > 0 && args(0) == "lda") {
      lda = new LDA(docs, numTopics, dict.lookup.size, 0.1, 0.1)
    } else {
      lda = new SparseLDA(docs, numTopics, dict.lookup.size, 0.1, 0.1)
    }

    0 until 100 foreach { i =>
      val t = System.nanoTime()
      println(s"Iteration $i")
      lda.run(1)
      val t2 = System.nanoTime()
      println(s"Iteration $i done")
      lda.computePhi().print(dict, lda.getTopicProbs())
      println(s"time = ${(t2 - t).toDouble / 1e+9} seconds")
    }
  }
}