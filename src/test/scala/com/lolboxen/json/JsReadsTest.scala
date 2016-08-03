package com.lolboxen.json

import com.lolboxen.json.implicits._
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by trent ahrens on 3/22/16.
  */
class JsReadsTest extends FlatSpec with Matchers {
  "integer reads" must "read int from json int" in {
    val node = Json.parse("""{"a":1}""")
    (node \ "a").asOpt[Int] shouldBe Some(1)
  }

  "integer reads" must "read int from json string" in {
    val node = Json.parse("""{"a":"1"}""")
    (node \ "a").asOpt[Int] shouldBe Some(1)
  }

  "bool reads" must "read bool from json bool" in {
    val node = Json.parse("""{"a":true}""")
    (node \ "a").asOpt[Boolean] shouldBe Some(true)
  }

  "bool reads" must "read bool from json int" in {
    val node = Json.parse("""{"a":1}""")
    (node \ "a").asOpt[Boolean] shouldBe Some(true)
  }

  "bool reads" must "read bool from json string" in {
    val node = Json.parse("""{"a":"true"}""")
    (node \ "a").asOpt[Boolean] shouldBe Some(true)
  }

  "map reads" must "read map of value type" in {
    val node = Json.parse("""{"a":"test","b":"test2"}""")
    node.asOpt[Map[String,String]] shouldBe Some(Map("a" -> "test", "b" -> "test2"))
  }

  "seq reads" must "read seq of value type" in {
    val node = Json.parse("""[1,2,3]""")
    node.asOpt[Seq[Int]] shouldBe Some(Seq(1,2,3))
  }

  "all as" must "read all values into seq" in {
    val node = Json.parse("""[1,2,3]""")
    node.explode.allAsOpt[Int] shouldBe Some(Seq(1,2,3))
  }
}
