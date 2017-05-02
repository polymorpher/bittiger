import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.{Alphabet, FeatureSequence, Instance, InstanceList}

/**
  * Created by Aaron Li (aaron@potatos.io) on 5/1/17.
  */

class MalletLDA(docs: Seq[Seq[Int]], numTopics: Int)
  extends LDA(docs, numTopics, 1, 1, 1) {
  val dict = new Alphabet
  dict.lookupIndex(0)
  for (doc <- docs; t <- doc) {
    dict.lookupIndex(t)
  }
  val isl = new InstanceList(dict, dict)
  for (doc <- docs) {
    val f = new FeatureSequence(dict)
    for (t <- doc) {
      f.add(t)
    }
    isl.add(new Instance(f, f, null, null))
  }
  val ptm = new ParallelTopicModel(numTopics)
  val docSize = docs.flatten.length
  ptm.addInstances(isl)

  override def run(numIters: Int): Unit = {
    ptm.setNumIterations(numIters)
    ptm.setOptimizeInterval(10)
    ptm.setNumThreads(1)
    ptm.estimate()
  }

  override def computePhi(): Array[Map[Int, Double]] = {
    ptm.getTopWords(50).map(_.zipWithIndex.map(e => e._1.asInstanceOf[Int] -> 1.0 / (e._2 + 1)).toMap)
  }

  override def computeTheta(): Array[Map[Int, Double]] = {
    docs.indices.map(d => ptm.getTopicProbabilities(d).zipWithIndex.map(e => e._2 -> e._1).toMap).toArray
  }

  override def getTopicProbs(): Array[Double] = {
    ptm.tokensPerTopic.map(e => e / docSize.toDouble)
  }

}
