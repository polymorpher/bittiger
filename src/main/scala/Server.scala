/**
  * Created by Aaron Li (aaron@potatos.io) on 5/1/17.
  */

import colossus._
import core._
import service._
import protocols.http._
import UrlParsing._
import HttpMethod._
import akka.actor.ActorSystem
import ann4s.AnnoyIndex
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.io.Source
import ann4s._


object NewsServer {

  implicit object ReviewBody extends HttpBodyEncoder[Seq[SNAPReview]] {
    val jsonHeader = HttpHeader("Content-Type", "application/json")

    def encode(json: Seq[SNAPReview]) = new HttpBody(mapper.writeValueAsBytes(json), Some(jsonHeader))
  }

  val jsonHeaders = HttpHeaders(HttpHeader("Content-Type", "application/json"))

  case class ModelledReview(review: SNAPReview, topics: Seq[Double])

  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  println("loading modelled reviews...")
  val (news, topics) = Source.fromFile("modelledReview.txt").getLines().map { line =>
    val e = mapper.readValue(line, classOf[ModelledReview])
    (e.review, e.topics)
  }.toArray.unzip
  println("building index....")
  val indexer = new AnnoyIndex(128, Euclidean)
  topics.zipWithIndex.foreach(t => indexer.addItem(t._2, t._1.toArray.map(_.toFloat)))
  indexer.build(-1)
  indexer.save("indexedReview.ann")
  println("indexing complete")

  class NewsService(context: ServerContext) extends HttpService(context) {
    def handle = {
      case request@Get on Root / "doc" / did =>
        val id = did.toInt
        if (id >= news.length || id < 0) {
          Callback.successful(request.badRequest(s"id must be in [0, ${news.length}), got $id"))
        } else {
          Callback.successful(request.ok(Seq(news(id))))
        }


      case request@Get on Root / "docs" =>
        Callback.successful(request.ok(news.take(10).toSeq))

      case request@Get on Root / "sim" / did =>
        val id = did.toInt
        if (id >= news.length || id < 0) {
          Callback.successful(request.badRequest(s"id must be in [0, ${news.length}), got $id"))
        } else {
          Callback.successful(request.ok(indexer.getNnsByItem(id, 5).map(e => news(e._1)).toSeq))
        }

    }
  }

  class NewsInitialiser(worker: WorkerRef) extends Initializer(worker) {

    def onConnect = context => new NewsService(context)

  }

  def main(args: Array[String]): Unit = {
    implicit val actorSystem = ActorSystem()
    implicit val io = IOSystem()
    Server.start("news-server", 9000) { worker => new NewsInitialiser(worker) }
  }

}