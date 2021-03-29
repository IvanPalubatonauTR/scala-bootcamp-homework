package com.evolutiongaming.async

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object AsyncHW extends App {

  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  args.foreach(url =>
    synchronized {
      for {
        body <- fetchPageBody(url)
        linkUrls <- findLinkUrls(body)
        serverNames <- fetchServerName(linkUrls)
      } yield serverNames.distinct.sorted.foreach(option => println(option.getOrElse("Not Found")))
    }
  )

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(urls: List[String]): Future[List[Option[String]]] = {
    import cats.implicits._
    urls.map(url => Future {
      println(s"Fetching server name header for $url")
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }).sequence
  }


  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}