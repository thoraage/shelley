package mary

import mary.shelley._
import org.specs2.mutable._

class SubGeneratorSpec extends Specification {

  "sub generator" should {
    "append sub iterators" in {
      cat("src/test/resources/testfile.txt") | each(echo) | asString must_== "hhhhhhh\nsssss\nttt\n"
    }
  }

}
