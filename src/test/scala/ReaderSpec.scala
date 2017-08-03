package bfa

import org.scalatest.{WordSpec, MustMatchers}

class ReaderSpec extends WordSpec with MustMatchers {
  "eof" must {
    "check eof if true" in {
      Reader("").eof must be(true)
    }

    "check eof not if true" in {
      Reader("foo").eof must not be (true)
    }
  }

  "next" must {
    "work fine with over using" in {
      Reader("a").next.next.next.backward.current must be(Some('a'))
    }
  }

  "current and next" must {
    "work fine" in {
      val re1 = Reader("abc")
      re1.current must be(Some('a'))
      val re2 = re1.next
      re2.current must be(Some('b'))
      val re3 = re2.next
      re3.current must be(Some('c'))
      val re4 = re3.next
      re4.current must be(None)
    }
  }

  "forward and backward" must {
    "work fine" in {
      val re1 = Reader("abc").backward
      re1.current must be(None)
      val re2 = re1.forward
      re2.current must be(Some('a'))
      val re3 = re2.next.next.next.backward.next
      re3.current must be(Some('b'))
      val re4 = re3.forward
      re4.current must be(Some('c'))
    }
  }
}
