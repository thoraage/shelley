package mary

import java.io.File
import io.Source
import java.util.zip.ZipFile

object shelley {
  type Formatter = Any => String
  type Generator[O] = Function0[Iterator[O]]
  trait Spawn[-I, O] extends Function[I, Iterator[O]]
  trait Filter[-I] extends Function1[I, Boolean]
  trait Mapper[-I, O] extends Function1[I, O]
  trait Sink[-I, O] extends Function1[I, O]
  type Aggregator[I] = (I, Function2[I, I, I])

  def stringify: Formatter = _ match {
    case n: Array[_] => n.map(stringify).mkString(",")
    case n: String => n
    case n => n.toString
  }

  case class select[S, T](f: S => T) extends Mapper[S, T] {
    def apply(s: S) = f(s)
  }
  case class join(delimeter: String = ";")  extends Mapper[Array[_], String] {
    def apply(a: Array[_]) = a.map(stringify).mkString(delimeter)
  }
  case class cut(private val delimeter: String = ";") extends Mapper[Any, Array[String]] {
    def apply(a: Any) = {
      val string = stringify(a)
      string.split(delimeter)
    }
  }
  case class sed(pattern: String, replacement: String) extends Mapper[Any, String] {
    val r = pattern.r
    def apply(a: Any) = {
      val string = stringify(a)
      r.replaceAllIn(string, replacement)
    }
  }
  case class findr(path: String = ".", private val fileType: FileTypes = FileTypes.All) extends Generator[File] {
    private var queue = Seq(new File(path)).iterator.buffered
    private def appendSubDirectory(dir: File) {
      queue = (queue ++ dir.listFiles().iterator).buffered
    }
    private def spool() {
      while (fileType == FileTypes.Dir && queue.hasNext && queue.head.isFile) queue.next()
      while (fileType == FileTypes.File && queue.hasNext && queue.head.isDirectory) {
        appendSubDirectory(queue.next())
      }
    }
    def apply() = new Iterator[File] {
      override def hasNext = {
        spool()
        queue.hasNext
      }
      override def next() = {
        spool()
        val file = queue.next()
        if (file.isDirectory) appendSubDirectory(file)
        file
      }
    }
    def directories = copy(fileType = FileTypes.Dir)
    def files = copy(fileType = FileTypes.File)
  }

  case class each[I, O](generator: I => Generator[O]) extends Spawn[I, O] {
    def apply(input: I) = generator(input)()
  }
  case class grep[I](pattern: String, private val inverted: Boolean = false) extends Filter[Any] {
    val regex = pattern.r
    def apply(input: Any) = regex.findFirstIn(stringify(input)).isDefined ^ inverted
    def invert = copy(inverted = !inverted)
  }

  case class unzip(file: File) extends Generator[(String, Long, Long)] {
    def apply() = sys.error("unzipping not implemented")
    def list = new Generator[(String, Long, Long)] {
      import collection.JavaConverters._
      def apply() = new ZipFile(file).entries.asScala.filter(!_.isDirectory).map(e => (e.getName, e.getSize, e.getTime)).toIterator
    }
  }
  case class cat(file: File) extends Generator[String] {
    def apply() = Source.fromFile(file).getLines
  }
  def echo(string: String) = new Generator[String] {
    def apply() = string.split("\n").iterator
  }
  case class ls(path: String = ".") extends Generator[File] {
    def apply() = {
      val file = new File(path)
      if (!file.exists)
        sys.error("File " + path + " not found")
      else if (file.isFile)
        List(file).iterator
      else
        file.listFiles().iterator
    }
    def verbose = new Generator[(File, Long)] {
      def apply() = ls(path)().map((f: File) => (f, f.length()))
    }
  }
  def print = new Sink[Any, Unit] {
    def apply(a: Any) { println(stringify(a)) }
  }
  def asString = new Sink[Any, String] {
    def apply(a: Any) = stringify(a)
  }
  def count = new Sink[Any, Int] {
    def apply(a: Any) = 1
  }

  implicit def stringToFile(s: String) = new File(s)
  implicit def generatorToStartPipe[O](generator: Generator[O]) = new StartPipe[O](generator)
  implicit val unitAggregator: Aggregator[Unit] = ((), (_: Unit, _: Unit) => ())
  implicit val stringAggregator: Aggregator[String] = ("", (s1, s2) => s1 + s2 + "\n")
  implicit val intAggregator: Aggregator[Int] = (0, (i1, i2) => i1 + i2)
}

import shelley._

trait OutputPipe[O] {
  def iterator: Iterator[O]
  def |[SO](sink: Sink[O, SO])(implicit aggregator: Aggregator[SO]): SO = {
    iterator.map(sink).foldLeft(aggregator._1)(aggregator._2)
  }
  def |[MO](mapper: Mapper[O, MO]): MapperPipe[O, MO] = new MapperPipe[O, MO](this, mapper)
  def |[SO](spawn: Spawn[O, SO]): SpawnPipe[O, SO] = new SpawnPipe[O, SO](this, spawn)
}

class StartPipe[O](generator: Generator[O]) extends OutputPipe[O] {
  def |(filter: Filter[O]): FilterPipe[O] = new FilterPipe[O](this, filter)
  def iterator = generator()
}

class FilterPipe[I](source: OutputPipe[I], filter: Filter[I]) extends OutputPipe[I] {
  def iterator = source.iterator.filter(value => filter(value))
}

class MapperPipe[I, O](source: OutputPipe[I], mapper: Mapper[I, O]) extends OutputPipe[O] {
  def iterator = source.iterator.map(mapper)
}

class SpawnPipe[I, O](source: OutputPipe[I], spawn: Spawn[I, O]) extends OutputPipe[O] {
  def iterator = source.iterator.flatMap(spawn)
}