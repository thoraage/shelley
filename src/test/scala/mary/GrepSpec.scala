package mary

import mary.Shelley._
import org.specs2.mutable._

class GrepSpec extends Specification {

  "grep" should {
    "filter out lines" in {
      ls | grep("arget") | asString must_== "target"
    }
  }

}
