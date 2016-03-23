package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode

/**
 * Created by trent ahrens on 4/23/15.
 */
trait Reads[A] {
  def reads(node: JsonNode): JsResult[A]
}

object Reads extends DefaultReads

trait DefaultReads {

  implicit object StringReads extends Reads[String] {
    override def reads(node: JsonNode): JsResult[String] =
      JsSuccess(node.asText())
  }

  implicit object IntegerReads extends Reads[Int] {
    override def reads(node: JsonNode): JsResult[Int] =
      JsSuccess(node.asInt())
  }

  implicit object LongReads extends Reads[Long] {
    override def reads(node: JsonNode): JsResult[Long] =
      JsSuccess(node.asLong())
  }

  implicit object DoubleReads extends Reads[Double] {
    override def reads(node: JsonNode): JsResult[Double] =
      JsSuccess(node.asDouble())
  }

  implicit object BooleanReads extends Reads[Boolean] {
    override def reads(node: JsonNode): JsResult[Boolean] =
      JsSuccess(node.asBoolean())
  }
}

sealed trait JsResult[+A] {
  def get: A
}

case class JsSuccess[A](value: A) extends JsResult[A] {
  override def get: A = value
}

case class JsError(reason: String) extends JsResult[Nothing] {
  override def get: Nothing = throw new Exception("JsError.get")
}
