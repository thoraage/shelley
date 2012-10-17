package mary

import java.io.File

object Shelley {

  type Generator[O] = Function0[Iterator[O]]
  type Filter[I] = Function1[I, Option[I]]
  type Sink[I, O] = Function1[I, O]

  val ls = LsCommand(".")

  def grep[I](pattern: String): Filter[I] = input => pattern.r.findFirstMatchIn(input.toString).map(_ => input)

  def print: Sink[Any, Unit] = a => println(a)

  def asString: Sink[Any, Unit] = a => a.toString

  implicit def generatorToStartPipe[O](generator: Generator[O]) = new StartPipe[O](generator)

  new StartPipe(ls).|(grep("ini")).|(print)
  ls | grep("ini") | print
}

import Shelley._

case class LsCommand(path: String) extends Generator[File] {
  def apply() = new File(path).listFiles().iterator
}

trait OutputPipe[O]

class StartPipe[O](generator: Generator[O]) extends OutputPipe[O] {
  def |(filter: Filter[O]) = new InterPipe(this, filter)
}

class InterPipe[I](output: OutputPipe[I], filter: Filter[I]) extends OutputPipe[I] {
  def |[O](sink: Sink[I, O]) = new EndPipe(this, sink)
}

class EndPipe[I, O](output: OutputPipe[I], sink: Sink[I, O]) {
  sys.error("Not implemented")
}
