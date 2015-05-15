package com.lolboxen.json

import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

/**
 * Created by trent ahrens on 5/15/15.
 */
class JsPathTest extends FlatSpec with Matchers {
  it should "follow key paths" in {
    val node = Json.parse("""{"a":{"b":2}}""")
    val sut = JsPath \ "a" \ "b"
    val actual = sut(node).as[Int]
    2 shouldBe actual
  }

  it should "select an array index" in {
    val node = Json.parse("""[1,2,3]""")
    val sut = JsPath \ 2
    val actual = sut(node).as[Int]
    3 shouldBe actual
  }

  it should "fan out" in {
    val node = Json.parse("""{"a":1,"b":2}""")
    val sut = JsPath *
    val actual = sut(node).map(n => n.as[Int])
    val expected = List(1,2)
    expected shouldBe actual
  }
}
