import java.io.File

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Aaron Li (aaron@potatos.io) on 4/28/17.
  */
class LDA(docs: Seq[Seq[Int]], numTopics: Int, numVocab: Int, alpha: Double, beta: Double) {
  // performance hack - pack two ints into a single long
  def long(a: Int, b: Int): Long = (a.toLong << 32) | b.toLong

  // unpack a long into two ints
  def ints(a: Long): (Int, Int) = ((a >> 32).toInt, (a & 0xffffffffL).toInt)

  // latent topic assignment for each word in each document.
  // initialise to be a random topic in [0, numTopics)
  protected[this] val z = docs.map { d => Array.fill(d.length)(util.Random.nextInt(numTopics)) }.toArray

  // initial topic counter for each topic in [0, numTopics)
  protected[this] val nkm = Map(z.flatten.groupBy(e => e).map { case (k, seq) => k -> seq.length }.toSeq: _*).withDefaultValue(0)

  // array form of topic counter, for better performance
  protected[this] val nk = (0 until numTopics).map(k => nkm(k)).toArray

  // initial document-topic counter for each document d and topic k
  protected[this] val ndk = mutable.LongMap(z.zipWithIndex.flatMap { case (zd, d) => zd.groupBy(z => z).map { case (zz, szz) => long(d, zz) -> szz.length } }: _*).withDefaultValue(0)

  // initial word-topic counter for each word w and topic k
  protected[this] val nwk = mutable.LongMap(docs.flatten.zip(z.flatten.toSeq).groupBy(e => e).map { case ((w, k), seq) => long(w, k) -> seq.length }.toSeq: _*).withDefaultValue(0)

  // hyperparameters and constants
  protected[this] val alphaSum = alpha * numTopics
  protected[this] val betaSum = beta * numVocab
  protected[this] val totalNumTokens = docs.flatten.length

  // random generator
  protected[this] val rr = new java.util.Random

  // pre-allocate buffer for computing topic probabilities
  protected[this] val probs = new Array[Double](numTopics)

  // Gibbs sampling, one step
  def sampleTopic(d: Int, l: Int): Int = {
    // current word
    val w = docs(d)(l)

    // normaliser
    var sum = 0.0
    var k = 0
    while (k < numTopics) {
      // predictive probability
      probs(k) = (ndk(long(d, k)) + alpha) * (nwk(long(w, k)) + beta) / (betaSum + nk(k))
      sum += probs(k)
      k += 1
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


