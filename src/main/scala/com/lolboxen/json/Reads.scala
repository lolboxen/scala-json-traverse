package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.TextNode

import scala.collection.JavaConverters._
import scala.util.Try

/**
 * Created by trent ahrens on 4/23/15.
 */
trait Reads[A] {
  def reads(node: JsonNode): JsResult[A]
}

object Reads extends DefaultReads

trait DefaultReads {

  implicit object JsonNodeReads extends Reads[JsonNode] {
    override def reads(node: JsonNode): JsResult[JsonNode] = JsSuccess(node)
  }

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

  implicit object BigIntReads extends Reads[BigInt] {
    override def reads(node: JsonNode): JsResult[BigInt] =
      parseBigInt(node.asText())
  }

  implicit object DoubleReads extends Reads[Double] {
    override def reads(node: JsonNode): JsResult[Double] =
      JsSuccess(node.asDouble())
  }

  implicit object BigDecimalReads extends Reads[BigDecimal] {
    override def reads(node: JsonNode): JsResult[BigDecimal] =
      parseBigDecimal(node.asText())
  }

  implicit object BooleanReads extends Reads[Boolean] {
    override def reads(node: JsonNode): JsResult[Boolean] =
      JsSuccess(node.asBoolean())
  }

  implicit def seqReads[V](implicit reads1: Reads[V]) = new Reads[Seq[V]] {
    override def reads(node: JsonNode): JsResult[Seq[V]] = {
      def loop(seq: Seq[V], elements: Stream[JsonNode]): JsResult[Seq[V]] = {
        elements match {
          case entry #:: tails =>
            reads1.reads(entry) match {
              case JsSuccess(value) =>
                loop(seq :+ value, tails)
              case JsError(reason) => JsError(reason)
            }
          case Stream() => JsSuccess(seq)
        }
      }

      loop(Nil, node.elements().asScala.toStream)
    }
  }

  implicit def setReads[V](implicit reads1: Reads[V]) = new Reads[Set[V]] {
    override def reads(node: JsonNode): JsResult[Set[V]] = {
      def loop(set: Set[V], elements: Stream[JsonNode]): JsResult[Set[V]] = {
        elements match {
          case entry #:: tails =>
            reads1.reads(entry) match {
              case JsSuccess(value) =>
                loop(set + value, tails)
              case JsError(reason) => JsError(reason)
            }
          case Stream() => JsSuccess(set)
        }
      }

      loop(Set.empty, node.elements().asScala.toStream)
    }
  }

  implicit def mapReads[K,V](implicit keyReads: Reads[K], valueReads: Reads[V]) = new MapReads[K,V](keyReads, valueReads)

  class MapReads[K,V](keyReads: Reads[K], valueReads: Reads[V]) extends Reads[Map[K,V]] {
    override def reads(node: JsonNode): JsResult[Map[K, V]] = {
      def loop(map: Map[K,V], seq: Stream[java.util.Map.Entry[String,JsonNode]]): JsResult[Map[K,V]] = {
        seq match {
          case entry #:: tails =>
            (keyReads.reads(TextNode.valueOf(entry.getKey)), valueReads.reads(entry.getValue)) match {
              case (JsSuccess(key), JsSuccess(value)) =>
                loop(map + (key -> value), tails)
              case (JsError(reason1), JsError(reason2)) => JsError(s"$reason1 : $reason2")
              case (_, JsError(reason)) => JsError(reason)
              case (JsError(reason), _) => JsError(reason)
            }
          case Stream() => JsSuccess(map)
        }
      }

      loop(Map.empty, node.fields().asScala.toStream)
    }
  }

  private def parseBigInt(s: String): JsResult[BigInt] =
    if (s.toLowerCase.contains("e"))
      parseBigDecimal(s).map(_.toBigInt())
    else
      Try(BigInt(s)).toOption match {
        case Some(bigint) => JsSuccess(bigint)
        case None => JsError(s"cannot read bigint value `$s`")
      }

  private def parseBigDecimal(s: String): JsResult[BigDecimal] = {
    val lows = s.toLowerCase
    if (lows.contains("e")) {
      val parts = lows.split("e")
      (Try(BigDecimal(parts(0))).toOption,Try(BigDecimal(10).pow(parts(1).toInt)).toOption) match {
        case (a, b) if a.isEmpty || b.isEmpty => JsError(s"cannot read bigdecimal value `$s`")
        case (Some(a), Some(b)) => JsSuccess(a * b)
      }
    }
    else
      Try(BigDecimal(s)).toOption match {
        case Some(bigdec) => JsSuccess(bigdec)
        case None => JsError(s"cannot read bigdecimal value `$s`")
      }
  }
}

sealed trait JsResult[+A] {
  def get: A
  def map[B](f: A => B): JsResult[B]
}

case class JsSuccess[A](value: A) extends JsResult[A] {
  override def get: A = value

  override def map[B](f: A => B): JsResult[B] = JsSuccess(f(value))
}

case class JsError(reason: String) extends JsResult[Nothing] {
  override def get: Nothing = throw new Exception("JsError.get")

  override def map[B](f: Nothing => B): JsResult[B] = this
}
