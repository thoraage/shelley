package mary

import mary.shelley._
import org.specs2.mutable._

class FindSpec extends Specification {

  "find" should {
    "list files and directories recursively" in {
      findr("src/test/resources") | count must_== 2
    }

    "with dir directive, should only list directories" in {
      findr("src/test/resources").directories | asString must_== "src/test/resources\n"
    }

    "with file directive, should only list files" in {
      findr("src/test/resources").files | asString must_== "src/test/resources/testfile.txt\n"
    }
  }

}
