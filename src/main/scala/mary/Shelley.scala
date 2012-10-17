package mary

import java.io.File

object Shelley {

  //type Column[T] = List[T]

  val ls = LsCommand(".")

  implicit def toPipe[T](cmd: Command[T]) = new StartPipe(cmd)
  implicit def toPipe[T](it: Iterator[T]) = new InterPipe(it)

  implicit def toStringify(f: String => Boolean): File => Boolean =
    file => f(file.toString)

  def grep(pattern: String): String => Boolean =
    (s: String) => pattern.r.findFirstMatchIn(s).isDefined

  def echo(a: Any) {
    println(a)
  }

  ls.|(grep("ini")).|(echo _)
  ls | grep("ini") | echo
}

trait Command[T] {
  def output: Iterator[T]
}

case class LsCommand(path: String) extends Command[File] {
  def output = new File(path).listFiles.iterator
}

trait Pipe[T] {
  def |(f: T => Unit)
}

class StartPipe[T](cmd: Command[T]) {
  def |(f: T => Unit) {
    cmd.output.foreach(f)
  }
  def |(f: T => Boolean): Pipe[T] = new InterPipe(this)
}

class InterPipe[T](pipe: Pipe) {
  def apply(cmd: Command[T]) = new StartPipe(cmd)

}
