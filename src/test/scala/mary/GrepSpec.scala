package mary

import mary.shelley._
import org.specs2.mutable._
import java.io.File

class GrepSpec extends Specification {

  "grep" should {
    "filter out lines" in {
      ls() | grep("arget") | asString must_== "./target"
    }
  }

}
