package mary

import mary.shelley._
import org.specs2.mutable._

class JoinSpec extends Specification {

  "join" should {
    "join a list with given delimiter" in {
      echo("a;b;c\nd;e") | cut(";") | join("|") | asString must_== "a|b|c\nd|e\n"
    }
  }

}
