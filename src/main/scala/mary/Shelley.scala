package mary

import java.io.File

object Shelley {
  type Generator[O] = Function0[Iterator[O]]
  type Filter[I] = Function1[I, Option[I]]
  type Sink[I, O] = Function1[I, O]
  type Aggregator[I] = Function2[I, I, I]

  val ls = LsCommand(".")
  def grep[I](pattern: String): Filter[I] = input => pattern.r.findFirstMatchIn(input.toString).map(_ => input)
  def print: Sink[Any, Unit] = a => println(a)
  def asString: Sink[Any, String] = a => a.toString

  implicit def generatorToStartPipe[O](generator: Generator[O]) = new StartPipe[O](generator)
  implicit val unitAggregator: Aggregator[Unit] = (_: Unit, _: Unit) => ()
  implicit val stringAggregator: Aggregator[String] = (s1, s2) => s1 + "\n" + s2
}

import Shelley._

case class LsCommand(path: String) extends Generator[File] {
  def apply() = new File(path).listFiles().iterator
}

trait OutputPipe[O] {
  def iterator: Iterator[O]
}

class StartPipe[O](generator: Generator[O]) extends OutputPipe[O] {
  def |(filter: Filter[O]) = new FilterPipe(this, filter)
  def iterator = generator()
}

class FilterPipe[IO](output: OutputPipe[IO], filter: Filter[IO]) extends OutputPipe[IO] {
  def |[O](sink: Sink[IO, O])(implicit aggregator: Aggregator[O]) = {
    iterator.map(sink).reduce(aggregator)
  }
  def iterator = output.iterator.filter(i => filter(i).isDefined)
}
