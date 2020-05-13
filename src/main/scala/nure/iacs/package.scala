package nure

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

package object iacs {
  implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global

  def parallelMap[A, B](collection: Seq[A], threadAmount: Int, mapOp: A => B): Seq[B] = {
    val inputPart = math.max(collection.length, (collection.length.toDouble / threadAmount).toInt)
    val freq = Future.sequence(for {
      i <- 0 until(collection.length, inputPart)
    } yield Future {
      collection.slice(i, i + inputPart).map(e => mapOp(e))
    }).map(_.flatten.toList)

    Await.result(freq, Duration.Inf)

    freq.value.get.get
  }
}
