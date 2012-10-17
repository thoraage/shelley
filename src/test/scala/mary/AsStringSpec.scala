package mary

import mary.shelley._
import org.specs2.mutable._

class AsStringSpec extends Specification {

  "asString" should {
    "convert input to string" in {
      (ls() | asString).length !== 0
    }
  }

}
