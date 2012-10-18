package mary

import org.specs2.mutable._
import mary.shelley._

class LsSpec extends Specification {

  "ls" should {
    "return list of files" in {
      ls("src") | count must_== 2
    }

    "with verbose, return list of files and other details" in {
      ls("src/test/resources").verbose | asString must_== "(src/test/resources/testfile.txt,8)\n"
    }
  }

}
