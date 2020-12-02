package com.github.myyk.advent2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class AdventSpec extends AnyFlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }
}
