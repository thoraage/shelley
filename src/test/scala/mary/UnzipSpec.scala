package mary

import org.specs2.mutable.Specification
import mary.shelley._

class UnzipSpec extends Specification {

  "unzip list" should {
    "output a stream of zip file entries" in {
      unzip("src/test/resources/testfile.zip").list | select[(String, Long, Long), String](_._1) | asString must_== "testfile.txt\nttt/testfile2.txt\n"
    }
  }

}
