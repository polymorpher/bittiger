import java.io.File

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Aaron Li (Polymorpher, polymorpher.darkiron@gmail.com) on 4/28/17.
  */
class LDA(docs: Seq[Seq[Int]], numTopics: Int, numVocab: Int, alpha: Double, beta: Double) {
  // performance hack - pack two ints into a single long
  def long(a: Int, b: Int): Long = (a.toLong << 32) | b.toLong

  // unpack a long into two ints
  def ints(a: Long): (Int, Int) = ((a >> 32).toInt, (a & 0xffffffffL).toInt)

  // latent topic assignment for each word in each document.
  // initialise to be a random topic in [0, numTopics)
  private val z = docs.map { d => Array.fill(d.length)(util.Random.nextInt(numTopics)) }.toArray

  // initial topic counter for each topic in [0, numTopics)
  private val nkm = Map(z.flatten.groupBy(e => e).map { case (k, seq) => k -> seq.length }.toSeq: _*).withDefaultValue(0)

  // array form of topic counter, for better performance
  private val nk = (0 until numTopics).map(k => nkm(k)).toArray

  // initial document-topic counter for each document d and topic k
  private val ndk = mutable.LongMap(z.zipWithIndex.flatMap { case (zd, d) => zd.groupBy(z => z).map { case (zz, szz) => long(d, zz) -> szz.length } }: _*).withDefaultValue(0)

  // initial word-topic counter for each word w and topic k
  private val nwk = mutable.LongMap(docs.flatten.zip(z.flatten.toSeq).groupBy(e => e).map { case ((w, k), seq) => long(w, k) -> seq.length }.toSeq: _*).withDefaultValue(0)

  // hyperparameters and constants
  private val alphaSum = alpha * numTopics
  private val betaSum = beta * numVocab
  private val totalNumTokens = docs.flatten.length

  // random generator
  private val rr = new java.util.Random

  // pre-allocate buffer for computing topic probabilities
  private val probs = new Array[Double](numTopics)

  // Gibbs sampling, one step
  def sampleTopic(d: Int, l: Int): Int = {
    // current word
    val w = docs(d)(l)

    // normaliser
    var sum = 0.0
    0 until numTopics foreach { k =>
      // predictive probability
      probs(k) = (ndk(long(d, k)) + alpha) * (nwk(long(w, k)) + beta) / (betaSum + nk(k))
      sum += probs(k)
    }

    // sample a single topic
    var r = rr.nextDouble * sum - probs(0)
    var t = 1
    while (r > 0 && t < numTopics) {
      r -= probs(t)
      t += 1
    }
    t - 1
  }

  def run(numIters: Int): Unit = {
    for (i <- 0 until numIters; d <- docs.indices; l <- docs(d).indices) {
      val k = z(d)(l)
      val w = docs(d)(l)
      // decrement counter related to current word and topic
      ndk(long(d, k)) -= 1; nwk(long(w, k)) -= 1; nk(k) -= 1
      val kk = sampleTopic(d, l)
      // reassign topic for current word
      z(d)(l) = kk
      //increment counter related to current word and newly sampled topic
      ndk(long(d, kk)) += 1; nwk(long(w, kk)) += 1; nk(kk) += 1
    }
  }

  // compute latent variable theta (used in the end)
  def computeTheta(): Array[Map[Int, Double]] =
    ndk.groupBy(kv => ints(kv._1)._1).map { case (d, m) => m.map { case (dk, c) => ints(dk)._2 -> (c + alpha) / (docs(d).length + alphaSum) }.toMap }.toArray

  // compute latent variable phi (used in the end)
  def computePhi(): Array[Map[Int, Double]] =
    nwk.groupBy { case (wk, _) => ints(wk)._2 }.map { case (k, m) =>
      m.map { case (wk, c) => ints(wk)._1 -> ((c + beta) / (nk(k) + betaSum)) }.toMap
    }.toArray

  // compute topic probabilities
  def getTopicProbs(): Array[Double] =
    nk.map(e => e.toDouble / totalNumTokens)
}

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

    // read vocabulary file and build dictionary
    Source.fromInputStream(getClass.getResourceAsStream(vf)).getLines().zipWithIndex.foreach { case (w, i) => dict.add(i + 1, w) }

    // read document data file and convert to 2D int array
    val docs = Source.fromInputStream(getClass.getResourceAsStream(df)).getLines().drop(3).toSeq.groupBy(e => e.split(" ")(0)).map(_._2.map(line => {
      val parts = line.split(" ")
      parts(1).toInt -> parts(2).toInt
    }).toMap).map(tokenMap => {
      tokenMap.flatMap { case (k, v) => Array.fill(v)(k) }.toSeq
    }).toSeq

    // initialise LDA instance, with hyperparameters and document data
    val lda = new LDA(docs, 8, dict.lookup.size, 0.1, 0.1)

    // run Gibbs sampling and print results for each iteration
    0 until 100 foreach { i =>
      println(s"Iteration $i")
      lda.run(1)
      lda.computePhi().print(dict, lda.getTopicProbs())
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
    val lda = new LDA(docs, 8, dict.lookup.size, 0.1, 0.1)
    0 until 100 foreach { i =>
      println(s"Iteration $i")
      lda.run(1)
      lda.computePhi().print(dict, lda.getTopicProbs())
    }
  }
}