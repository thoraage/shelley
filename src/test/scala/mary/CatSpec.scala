package mary

import mary.shelley._
import org.specs2.mutable._

class CatSpec extends Specification {

  "cat" should {
    "output a stream of lines" in {
      cat("src/test/resources/testfile.txt") | asString must_== "hhhhhhh\nsssss\nttt\n"
    }
  }

}
