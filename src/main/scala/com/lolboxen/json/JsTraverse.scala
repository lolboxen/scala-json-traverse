package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode

import scala.collection.JavaConversions._
import scala.language.implicitConversions

/**
 * Created by trent ahrens on 5/15/15.
 */
object JsTraverse {
  val empty = JsTraverse(List.empty)

  def apply(node: JsonNode): JsTraverse = node match {
    case n: ArrayNode => JsTraverse(n.elements().toList)
    case n => JsTraverse(List(n))
  }

  def apply(node: Option[JsonNode]): JsTraverse = node match {
    case Some(n) => JsTraverse(n)
    case None => empty
  }
}

case class JsTraverse(nodes: Seq[JsonNode]) {
  import JsTraverse._

  def \(key: String): JsTraverse = nodes match {
    case h :: tail => JsTraverse(Option(h.get(key))).append(JsTraverse(tail) \ key)
    case _ => empty
  }

  def \(index: Int) = JsTraverse(nodes.drop(index).take(1))

  def \(filter: JsFilter): JsTraverse = filter(this)

  def * = JsTraverse(nodes.flatMap(n => n.elements()))

  def length: Int = nodes.length

  def map[A](f: JsonNode => A): Seq[A] = nodes.map(f)

  def filter(f: JsonNode => Boolean): JsTraverse = JsTraverse(nodes.filter(f))

  def diff(t: JsTraverse): JsTraverse = JsTraverse(nodes.diff(t.nodes))

  def and(t: JsTraverse): JsTraverse = JsTraverse(nodes.filter(t.nodes.contains(_)))

  def append(t: JsTraverse): JsTraverse = JsTraverse(nodes ++ t.nodes)

  def as[A](implicit reads: Reads[A]): A = reads.reads(nodes.head) match {
    case JsSuccess(value) => value
    case JsError(error) => throw new Exception(error)
  }

  def asOpt[A](implicit reads: Reads[A]): Option[A] = nodes.headOption flatMap {
    case n => reads.reads(n) match {
      case JsSuccess(value) => Some(value)
      case _ => None
    }
  }

  def validate[A](implicit reads: Reads[A]): JsResult[A] = nodes.headOption match {
    case Some(n) => reads.reads(n)
    case _ => JsError("path not found")
  }
}
