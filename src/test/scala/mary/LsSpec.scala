package mary

import org.specs2.mutable._
import mary.shelley._

class LsSpec extends Specification {

  "ls" should {
    "return list of files" in {
      ls("src") | count must_== 2
    }


  }

}
