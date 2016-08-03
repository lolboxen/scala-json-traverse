package com.lolboxen.json

import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps
import com.lolboxen.json.implicits._

/**
 * Created by trent ahrens on 5/15/15.
 */
class JsPathTest extends FlatSpec with Matchers {
  it should "follow key paths" in {
    val node = Json.parse("""{"a":{"b":2}}""")
    val sut = JsPath \ "a" \ "b"
    val actual = sut(node).asOpt[Int]
    Some(2) shouldBe actual
  }

  it should "select an array index" in {
    val node = Json.parse("""[1,2,3]""")
    val sut = JsPath \ 2
    val actual = sut(node).asOpt[Int]
    Some(3) shouldBe actual
  }
}
