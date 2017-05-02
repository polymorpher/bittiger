import scala.collection.mutable

/**
  * Created by Aaron Li (aaron@potatos.io) on 5/1/17.
  */

class SparseLDA(docs: Seq[Seq[Int]], numTopics: Int, numVocab: Int, alpha: Double, beta: Double)
  extends BasicLDA(docs, numTopics, numVocab, alpha, beta) {
  var (bs, bq, br) = (0.0, 0.0, 0.0)
  val (as, aq, ar) = (Array.fill(numTopics)(0.0), mutable.LongMap[Double]().withDefaultValue(0), mutable.LongMap[Double]().withDefaultValue(0))
  val (mdk, mwk) = (Array.fill(docs.length)(mutable.Set[Int]()), Array.fill(numVocab + 1)(mutable.Set[Int]()))
  ndk.foreach { case (dk, n) =>
    val (d, k) = ints(dk)
    mdk(d) += k
  }
  nwk.foreach { case (wk, n) =>
    val (w, k) = ints(wk)
    mwk(w) += k
  }

  private def computeAS(): Unit = (0 until numTopics).foreach { k =>
    bs = 0.0
    as(k) = (alpha * beta) / (betaSum + nk(k))
    bs += as(k)
  }

  private def updateAS(k: Int): Unit = {
    bs -= as(k)
    as(k) = (alpha * beta) / (betaSum + nk(k))
    bs += as(k)
  }

  private def computeAR(d: Int): Unit = {
    ar.clear()
    br = 0.0
    mdk(d).foreach { k =>
      ar(k) = ndk(long(d, k)) * beta / (betaSum + nk(k))
      br += ar(k)
    }
  }

  private def updateAR(d: Int, k: Int): Unit = {
    br -= ar(k)
    ar(k) = ndk(long(d, k)) * beta / (betaSum + nk(k))
    br += ar(k)
  }

  private def computeAQ(d: Int, w: Int): Unit = {
    bq = 0.0
    aq.clear()
    mwk(w).foreach { k =>
      aq(k) = nwk(long(w, k)) * (alpha + ndk(long(d, k))) / (betaSum + nk(k))
      bq += aq(k)
    }
  }

  override def run(numIters: Int): Unit = {
    var (i, d, l) = (0, 0, 0)
    while (i < numIters) {
      computeAS()
      while (d < docs.length) {
        computeAR(d)
        while (l < docs(d).length) {
          val k = z(d)(l)
          val w = docs(d)(l)
          // decrement counter related to current word and topic
          val dk = long(d, k)
          val wk = long(w, k)
          ndk(dk) -= 1
          if (ndk(dk) == 0) {
            mdk(d) -= k
          }
          nwk(wk) -= 1
          if (nwk(wk) == 0) {
            mwk(w) -= k
          }
          nk(k) -= 1
          updateAS(k)
          updateAR(d, k)
          computeAQ(d, w)
          val kk = sampleTopic(d, l)
          // reassign topic for current word
          z(d)(l) = kk
          //increment counter related to current word and newly sampled topic
          val dkk = long(d, kk)
          val wkk = long(w, kk)
          if (ndk(dkk) == 0) {
            mdk(d) += k
          }
          ndk(dkk) += 1
          if (nwk(wkk) == 0) {
            mwk(w) += k
          }
          nwk(wkk) += 1
          nk(kk) += 1
          updateAS(k)
          updateAR(d, k)
          l += 1
        }
        d += 1
        l = 0
      }
      i += 1
      d = 0
    }
  }

  override def sampleTopic(d: Int, l: Int): Int = {
    var sum = bq + br + bs
    var r = rr.nextDouble * sum
    var i = 0
    if (r < bq) {
      //sample from q bucket
      val entries = aq.toSeq
      while (r > 0 && i < entries.length) {
        r -= entries(i)._2
        i += 1
      }
      entries(i - 1)._1.toInt
    } else if (r < br + bq) {
      //sample from r bucket
      r -= bq
      val entries = ar.toSeq
      while (r > 0 && i < entries.length) {
        r -= entries(i)._2
        i += 1
      }
      entries(i - 1)._1.toInt
    } else {
      r -= (bq + br)
      //sample from s bucket
      while (r > 0 && i < numTopics) {
        r -= as(i)
        i += 1
      }
      i - 1
    }
  }
}