package com.github.myyk.advent2018

import org.scalatest._

class AdventSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }
}
