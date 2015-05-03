package ch1

import org.specs2.mutable._

class SampleSpec extends Specification {
  "Sample#foo()" should  {
    "return string 'foo'" in {
      new Sample().foo() must_== "foo"
    }
  }
}
