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
      if (node.isValueNode) JsSuccess(node.asText())
      else JsError("cannot read as String")
  }

  implicit object IntegerReads extends Reads[Int] {
    override def reads(node: JsonNode): JsResult[Int] =
      if (node.isNumber) JsSuccess(node.asInt())
      else JsError("cannot read as Int")
  }

  implicit object LongReads extends Reads[Long] {
    override def reads(node: JsonNode): JsResult[Long] =
      if (node.isNumber) JsSuccess(node.asLong())
      else JsError("cannot read as Long")
  }

  implicit object DoubleReads extends Reads[Double] {
    override def reads(node: JsonNode): JsResult[Double] =
      if (node.isNumber) JsSuccess(node.asDouble())
      else JsError("cannot read as Double")
  }

  implicit object BooleanReads extends Reads[Boolean] {
    override def reads(node: JsonNode): JsResult[Boolean] =
      if (node.isBoolean || node.isInt) JsSuccess(node.asBoolean())
      else JsError("cannot read as Boolean")
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
