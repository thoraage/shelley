package mary

import java.io.File

object shelley {
  type Generator[O] = Function0[Iterator[O]]
  type Filter[I] = Function1[I, Option[I]]
  type Sink[I, O] = Function1[I, O]
  type Aggregator[I] = Function2[I, I, I]

  case class findr(path: String = ".") extends Generator[File] {
    var queue = new File(".").listFiles().iterator
    def apply() = new Iterator[File] {
      override def hasNext = queue.hasNext
      override def next() = {
        val file = queue.next()
        if (file.isDirectory) {
          queue ++= file.listFiles().iterator
        }
        file
      }
    }
  }
  case class ls(path: String = ".") extends Generator[File] {
    def apply() = new File(path).listFiles().iterator
  }
  def grep(pattern: String): Filter[String] = input => pattern.r.findFirstMatchIn(input).map(_ => input)
  def print: Sink[Any, Unit] = a => println(a)
  def asString: Sink[Any, String] = a => a.toString
  def count: Sink[Any, Int] = a => 1

  implicit def generatorToStartPipe[O](generator: Generator[O]) = new StartPipe[O](generator)
  implicit val unitAggregator: Aggregator[Unit] = (_: Unit, _: Unit) => ()
  implicit val stringAggregator: Aggregator[String] = (s1, s2) => s1 + "\n" + s2
  implicit val intAggregator: Aggregator[Int] = (i1, i2) => i1 + i2
}

import shelley._

trait OutputPipe[O] {
  def iterator: Iterator[O]
  def |[SO](sink: Sink[O, SO])(implicit aggregator: Aggregator[SO]) = {
    iterator.map(sink).reduce(aggregator)
  }
}

class StartPipe[O](generator: Generator[O]) extends OutputPipe[O] {
  def |(filter: Filter[String]) = new FilterPipe[O](this, filter)
  def iterator = generator()
}

class FilterPipe[I](output: OutputPipe[I], filter: Filter[String]) extends OutputPipe[I] {
  def iterator = output.iterator.filter(value => filter(value.toString).isDefined)
}
