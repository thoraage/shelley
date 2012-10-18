package mary

import org.specs2.mutable._
import mary.shelley._

class SedSpec extends Specification {

  "sed" should {
    "substitute strings" in {
      ls() | grep("arget") | sed("\\/", "x") | sed("ar", "y") | asString must_== ".xtyget"
    }
  }

}
