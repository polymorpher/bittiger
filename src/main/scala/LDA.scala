import scala.collection.mutable
import scala.io.Source

/**
  * Created by Aaron Li (Polymorpher, polymorpher.darkiron@gmail.com) on 4/28/17.
  */
class LDA(docs: Seq[Seq[Int]], numTopics: Int, numVocab: Int, alpha: Double, beta: Double) {
  def long(a: Int, b: Int): Long = (a.toLong << 32) | b.toLong

  def ints(a: Long): (Int, Int) = ((a >> 32).toInt, (a & 0xffffffffL).toInt)

  private val z = docs.map { d => Array.fill(d.length)(util.Random.nextInt(numTopics)) }.toArray
  private val nkm = Map(z.flatten.groupBy(e => e).map { case (k, seq) => k -> seq.length }.toSeq: _*).withDefaultValue(0)
  private val nk = (0 until numTopics).map(k => nkm(k)).toArray
  private val ndkm = mutable.LongMap(z.zipWithIndex.flatMap { case (zd, d) => zd.groupBy(z => z).map { case (zz, szz) => long(d, zz) -> szz.length } }: _*).withDefaultValue(0)
  private val ndk = docs.indices.map { d => (0 until numTopics).map { k => ndkm(long(d, k)) }.toArray }.toArray
  //z.map { d => mutable.HashMap(d.groupBy(e => e).map(e => e._1 -> e._2.length).toSeq: _*).withDefaultValue(0) }
  private val nwkm = mutable.LongMap(docs.flatten.zip(z.flatten.toSeq).groupBy(e => e).map { case ((w, k), seq) => long(w, k) -> seq.length }.toSeq: _*).withDefaultValue(0)
  private val nwk = (0 until numVocab+1).map { w => (0 until numTopics).map { k => nwkm(long(w, k)) }.toArray }.toArray
  private val alphaSum = alpha * numTopics
  private val betaSum = beta * numVocab
  private val totalNumTokens = docs.flatten.length
  private val rr = new java.util.Random
  private val probs = new Array[Double](numTopics)

  def sampleTopic(d: Int, l: Int): Int = {
    val w = docs(d)(l)
    var sum = 0.0
    0 until numTopics foreach { k =>
      probs(k) = (ndk(d)(k) + alpha) * (nwk(w)(k) + beta) / (betaSum + nk(k))
      sum += probs(k)
    }
    var cumProb = 0.0
    var r = rr.nextDouble * sum
    var t = 0
    do {
      cumProb += probs(t)
      t += 1
    } while (cumProb < r && t < numTopics)
    t - 1
  }

  def run(numIters: Int): Unit = {
    for (i <- 0 until numIters; d <- docs.indices; l <- docs(d).indices) {
      val k = z(d)(l)
      val w = docs(d)(l)
      ndk(d)(k) -= 1; nwk(w)(k) -= 1; nk(k) -= 1
      val kk = sampleTopic(d, l)
      z(d)(l) = kk
      ndk(d)(kk) += 1; nwk(w)(kk) += 1; nk(kk) += 1
    }
  }

  def computeTheta(): Array[Map[Int, Double]] =
    ndk.zipWithIndex.map { case (ar, d) => ar.zipWithIndex.map { case (n, k) => k -> ((n + alpha) / (docs(d).length + alphaSum)) }.toMap }

  //    ndk.groupBy(kv => ints(kv._1)._1).map { case (d, m) => m.map { case (dk, c) => ints(dk)._2 -> (c + alpha) / (docs(d).length + alphaSum) }.toMap }.toArray

  //{e=> case (m, d) => m.mapValues(e => (e + alpha) / (docs(d).length + alphaSum)).toMap }


  def computePhi(): Array[Map[Int, Double]] =
    nwk.transpose.zipWithIndex.map { case (ar, k) => ar.zipWithIndex.map { case (n, w) => w -> ((n + beta) / (nk(k) + betaSum)) }.toMap }
  //    nwk.groupBy { case (wk, _) => ints(wk)._2 }.map { case (k, m) =>
  //      m.map { case (wk, c) => ints(wk)._1 -> ((c + beta) / (nk(k) + betaSum)) }.toMap
  //    }.toArray


  def getTopicProbs(): Array[Double] =
    nk.map(e => e.toDouble / totalNumTokens)
}


object BOWReader {

  implicit class PrinterUtil(phi: Array[Map[Int, Double]]) {
    def print(dict: Dict, tp: Array[Double]) = println(
      tp.zipWithIndex.zip(phi).sortBy(-_._1._1).map { case ((pk, k), m) =>
        s"Topic $k, prob: $pk: " + m.toSeq.sortBy(-_._2).take(10).map { case (w, pw) => s"[${dict.lookup(w)}, ${pw.formatted("%.3f")}]" }.mkString(", ")
      }.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    val vf = s"/bow/vocab.${args(0)}.txt"
    val df = s"/bow/docword.${args(0)}.txt"
    val dict = new Dict()
    Source.fromInputStream(getClass.getResourceAsStream(vf)).getLines().zipWithIndex.foreach { case (w, i) => dict.add(i + 1, w) }
    val docs = Source.fromInputStream(getClass.getResourceAsStream(df)).getLines().drop(3).toSeq.groupBy(e => e.split(" ")(0)).map(_._2.map(line => {
      val parts = line.split(" ")
      parts(1).toInt -> parts(2).toInt
    }).toMap).map(tokenMap => {
      tokenMap.flatMap { case (k, v) => Array.fill(v)(k) }.toSeq
    }).toSeq

    val lda = new LDA(docs, 8, dict.lookup.size, 0.1, 0.1)
    0 until 20 foreach { i =>
      println(s"Iteration $i")
      lda.run(5)
      lda.computePhi().print(dict, lda.getTopicProbs())
    }
  }
}

