package mary

import java.io.File

object shelley {
  type Generator[O] = Function0[Iterator[O]]
  trait Filter[I] extends Function1[I, Boolean]
  trait Mapper[-I, O] extends Function1[I, O]
  trait Sink[-I, O] extends Function1[I, O]
  type Aggregator[I] = Function2[I, I, I]

  case class sed(pattern: String, replacement: String) extends Mapper[Any, String] {
    val r = pattern.r
    def apply(a: Any) = {
      val string: String = a.toString
      r.replaceAllIn(string, replacement)
    }
  }
  case class findr(path: String = ".") extends Generator[File] {
    private var queue = new File(".").listFiles().iterator
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
    /*def verbose = new Generator[(File, Int)] {
      apply
    } */
  }
  case class grep(pattern: String, private val inverted: Boolean = false) extends Filter[String] {
    val regex = pattern.r
    def apply(input: String) = regex.findFirstIn(input).isDefined ^ inverted
    def invert = copy(inverted = !inverted)
  }
  def print = new Sink[Any, Unit] {
    def apply(a: Any) { println(a) }
  }
  def asString = new Sink[Any, String] {
    def apply(a: Any) = a.toString
  }
  def count = new Sink[Any, Int] {
    def apply(a: Any) = 1
  }

  implicit def generatorToStartPipe[O](generator: Generator[O]) = new StartPipe[O](generator)
  implicit val unitAggregator: Aggregator[Unit] = (_: Unit, _: Unit) => ()
  implicit val stringAggregator: Aggregator[String] = (s1, s2) => s1 + "\n" + s2
  implicit val intAggregator: Aggregator[Int] = (i1, i2) => i1 + i2
}

import shelley._

trait OutputPipe[O] {
  def iterator: Iterator[O]
  def |[SO](sink: Sink[O, SO])(implicit aggregator: Aggregator[SO]): SO = {
    iterator.map(sink).reduce(aggregator)
  }
  def |[MO](mapper: Mapper[O, MO]): MapperPipe[O, MO] = new MapperPipe[O, MO](this, mapper)
}

class StartPipe[O](generator: Generator[O]) extends OutputPipe[O] {
  def |(filter: Filter[String]): FilterPipe[O] = new FilterPipe[O](this, filter)
  def iterator = generator()
}

class FilterPipe[I](source: OutputPipe[I], filter: Filter[String]) extends OutputPipe[I] {
  def iterator = source.iterator.filter(value => filter(value.toString))
}

class MapperPipe[I, O](source: OutputPipe[I], mapper: Mapper[I, O]) extends OutputPipe[O] {
  def iterator = source.iterator.map(mapper)
}