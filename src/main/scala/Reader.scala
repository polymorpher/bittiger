import java.io.File

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.collection.immutable.{HashMap, HashSet}
import scala.collection.parallel._
import java.io.{BufferedWriter, File, FileWriter}

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Aaron Li (Polymorpher, polymorpher.darkiron@gmail.com) on 4/28/17.
  */
case class SNAPReview(reviewerID: String, asin: String, reviewerName: String, helpful: Seq[Int], reviewText: String, overall: Double, summary: String, unixReviewTime: Long, reviewTime: String)

class SNAPReader {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def read(file: File): Seq[SNAPReview] = {
    scala.io.Source.fromFile(file).getLines().map { line =>
      mapper.readValue(line, classOf[SNAPReview])
      //      val jj = j.asInstanceOf[Map[String, Any]]
      //      jj("reviewText").asInstanceOf[String]
    }.filter(e => e.reviewText != "").toList
  }

  def read(f: String): Seq[SNAPReview] = read(new File(f))
}

object SNAPReaderDemo {
  def main(args: Array[String]): Unit = {
    val f = new File(getClass.getResource("/SanDiskUltra64GB.txt").toURI)
    val r = new SNAPReader
    println(r.read(f).head)
  }
}

class Dict {
  val lookup = new mutable.HashMap[Int, String]
  val rlookup = new mutable.HashMap[String, Int]

  def add(i: Int, w: String): Unit = {
    this.synchronized {
      if (lookup.contains(i)) {
        throw new Exception(s"$i exists")
      }
      rlookup.put(w, i)
      lookup.put(i, w)
    }
  }
}

object NLPCore {
  private val proc = new FastNLPProcessor(withChunks = false, withDiscourse = ShallowNLPProcessor.NO_DISCOURSE)
  private var stopwords: HashSet[String] = _

  private def loadStopWords(): Unit = {
    val stream = getClass.getResourceAsStream("/stopwords")
    val lines = scala.io.Source.fromInputStream(stream).getLines
    stopwords = HashSet(lines.toSeq: _*)
  }

  loadStopWords()

  def parse(text: String): Map[String, Int] = {
    val tokens = new ArrayBuffer[String]
    val d = proc.mkDocument(text)
    proc.tagPartsOfSpeech(d)
    proc.lemmatize(d)
    //    proc.recognizeNamedEntities(d)
    for (s <- d.sentences) {
      val lemmas = s.lemmas.get.map(_.toLowerCase)
      //      val ner = s.entities.get
      val pos = s.tags.get
      //      println(s.words.toSeq, lemmas.toSeq, ner.toSeq, pos.toSeq)
      for (i <- s.words.indices) {
        if (!stopwords.contains(lemmas(i))) {
          tokens += s"${lemmas(i)}#${pos(i)}"
        }
      }
    }
    tokens.groupBy(e => e).map(e => e._1 -> e._2.length)

  }

  def main(args: Array[String]): Unit = {
    println(parse("John Smith went to China. He visited Beijing, on January 10th, 2013."))
  }
}
