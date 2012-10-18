package mary

import mary.shelley._
import org.specs2.mutable._

class GrepSpec extends Specification {

  "grep" should {
    "filter matching lines" in {
      ls() | grep("arget") | asString must_== "./target"
    }

    "if inverted, filter non-matching lines" in {
      ls() | (grep("arget").invert) | asString must not contain "./target"
    }
  }

}
