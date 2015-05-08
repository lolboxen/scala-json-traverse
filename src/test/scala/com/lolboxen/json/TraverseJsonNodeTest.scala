package com.lolboxen.json

import com.fasterxml.jackson.databind.ObjectMapper
import com.lolboxen.json.TraverseJsonNode._
import com.lolboxen.test.UnitSpec

import scala.language.postfixOps

class TraverseJsonNodeTest extends UnitSpec {

  val mapper = new ObjectMapper()
  val node = mapper.readTree("""{"a":{"b":{"c":10,"d":[1,2,3],"e":[{"a":"1"},{"a":"1","b":1},{"b":"2"}]}}}""")
  val node2 = mapper.readTree("""{"c": [{"a": {"b": {"a":"1"}, "c": "2"}},{"a": {"b": {"a":"2"}, "c":"1"}}]}""")

  it should "traverse json node when keys exist" in {
    (node \ "a" \ "b" \ "c").asOpt[Int] shouldBe Some(10)
    (node \ "a" \ "b" \ "d" \ 0).asOpt[String] shouldBe Some("1")
  }

  it should "continue to traverse when key isn't found" in {
    node \ "a" \ "c" shouldBe Empty
    node \ "a" \ 1 shouldBe Empty
    node \ "a" \ "d" \ "f" shouldBe Empty
  }

  it should "filter array" in {
    (node \ "a" \ "b" \ "e" \ ("a" `=` "1")).length shouldBe 2
    (node \ "a" \ "b" \ "e" \ ("a" `=` "1") \ 1 \ "b").asOpt[Int] shouldBe Some(1)
  }

  it should "map a list of nodes" in {
    (node \ "a" \ "b" \ "d" map(_.asInt()+1)) shouldBe List(2,3,4)
  }

  it should "work with reads" in {
    (node \ "a" \ "b" \ "c").asOpt[String] shouldBe Some("10")
  }

  it should "work with disjunction filters" in {
    (node \ "a" \ "b" \ "e" \ (("b" `=` "1") || ("b" `=` "2"))).length shouldBe 2
  }

  it should "work with conjunction filters" in {
    (node \ "a" \ "b" \ "e" \ (("a" `=` "1") && ("b" `=` "1"))).length shouldBe 1
  }

  it should "work with negative filters" in {
    (node \ "a" \ "b" \ "e" \ !("a" `=` "1")).length shouldBe 1
  }

  it should "deep filter structures" in {
    (node2 \ "c" \ "a" \ ("b" \ ("a" `=` "1")) \ "c").asOpt[String] shouldBe Some("2")
  }

  it should "append and filter two different nodes" in {
    ((node \ "a" \ "b" \ "e" \ ("b" `=` "1")).append(node2 \ "c" \ "a" \ ("c" `=` "1")) \ "b" \ 1 \ "a").asOpt[String] shouldBe Some("2")
  }

  it should "treat object as array" in {
    TraverseJson(node).* \ 0 \ "b" shouldBe node \ "a" \ "b"
  }
}
