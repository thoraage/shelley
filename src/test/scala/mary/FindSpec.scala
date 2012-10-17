package mary

import mary.shelley._
import org.specs2.mutable._

class FindSpec extends Specification {

  "find" should {
    "list files and directories recursively" in {
      findr("src/main/scala") | count !== 0
    }
  }

}
