package mary

import org.specs2.mutable._
import mary.shelley._

class CutSpec extends Specification {

  "cut" should {
    "clip a string in subparts" in {
      echo("a;b;c\nd;e") | cut(";") | asString must_== "a,b,c\nd,e\n"
    }
  }

}
