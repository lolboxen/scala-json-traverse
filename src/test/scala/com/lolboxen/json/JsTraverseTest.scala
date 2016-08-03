package com.lolboxen.json

import com.fasterxml.jackson.databind.node.TextNode
import com.lolboxen.json.implicits._
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

class JsTraverseTest extends FlatSpec with Matchers {

  val node = Json.parse("""{"a":{"b":{"c":10,"d":[1,2,3],"e":[{"a":"1"},{"a":"1","b":1},{"b":"2"}]}}}""")
  val node2 = Json.parse("""{"c": [{"a": {"b": {"a":"1"}, "c": "2"}},{"a": {"b": {"a":"2"}, "c":"1"}}]}""")

  it should "traverse json node when keys exist" in {
    (node \ "a" \ "b" \ "c").asOpt[Int] shouldBe Some(10)
    (node \ "a" \ "b" \ "d" \ 0).asOpt[String] shouldBe Some("1")
  }

  it should "traverse json node when nodes are a stream" in {
    val sut = JsTraverse(Stream.apply(node, node), JsPath) \ "a" \ "b" \ "c"
    sut.allAs[Int] shouldBe Seq(10,10)
  }

  it should "continue to traverse when key isn't found" in {
    (node \ "a" \ "c").length shouldBe 0
    (node \ "a" \ 1).length shouldBe 0
    (node \ "a" \ "d" \ "f").length shouldBe 0
  }

  it should "work with reads" in {
    (node \ "a" \ "b" \ "c").asOpt[String] shouldBe Some("10")
  }

  it should "work with disjunction filters" in {
    ((node \ "a" \ "b" \ "e").explode \ (("b" `=` "1") || ("b" `=` "2"))).length shouldBe 2
  }

  it should "work with conjunction filters" in {
    ((node \ "a" \ "b" \ "e").explode \ (("a" `=` "1") && ("b" `=` "1"))).length shouldBe 1
  }

  it should "work with negative filters" in {
    (node \ "a" \ "b" \ "e" \ !("a" `=` "1")).length shouldBe 1
  }

  it should "deep filter structures" in {
    ((node2 \ "c").explode \ "a" \ ("b" \ ("a" `=` "1")) \ "c").asOpt[String] shouldBe Some("2")
  }

  it should "append and filter two different nodes" in {
    ((node \ "a" \ "b" \ "e" \ 0).append(node2 \"c" \ 0 \ "a" \ "b") \ "a").allAsOpt[Int] shouldBe Some(Seq(1,1))
  }
}
