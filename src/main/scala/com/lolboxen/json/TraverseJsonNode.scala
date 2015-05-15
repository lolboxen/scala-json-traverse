package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode

import scala.collection.JavaConversions._
import scala.language.postfixOps

object TraverseJsonNode {
  import com.lolboxen.json._

  val Empty = TraverseJson(List.empty)

  object TraverseJson {
    def apply(node: JsonNode): TraverseJson = node match {
      case n: ArrayNode => TraverseJson(n.elements().toList)
      case n => TraverseJson(List(n))
    }

    def apply(node: Option[JsonNode]): TraverseJson = node match {
      case Some(n) => TraverseJson(n)
      case None => Empty
    }
  }

  case class TraverseJson(nodes: Seq[JsonNode]) {
    def \(name: String): TraverseJson = nodes match {
      case h :: tail => (h \ name).append(TraverseJson(tail) \ name)
      case _ => Empty
    }

    def \(index: Int) = TraverseJson(nodes.drop(index).take(1))

    def \(f: Filter): TraverseJson = f.filter(this)

    def * = TraverseJson(nodes.flatMap(n => n.elements()))

    def length: Int = nodes.length

    def map[A](f: JsonNode => A): Seq[A] = nodes.map(f)

    def filter(f: JsonNode => Boolean): TraverseJson = TraverseJson(nodes.filter(f))

    def diff(t: TraverseJson): TraverseJson = TraverseJson(nodes.diff(t.nodes))

    def and(t: TraverseJson): TraverseJson = TraverseJson(nodes.filter(t.nodes.contains(_)))

    def append(t: TraverseJson): TraverseJson = TraverseJson(nodes ++ t.nodes)

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

  case class SimpleFilter(key: String, value: String) extends Filter {
    override def filter(t: TraverseJson) = t.filter(n => (n \ key).asOpt[String] == Some(value))
  }

  case class ComposableAndFilter(f: Filter, g: Filter) extends Filter {
    override def filter(t: TraverseJson) = (t \ f) and (t \ g)
  }

  case class ComposableOrFilter(f: Filter, g: Filter) extends Filter {
    override def filter(t: TraverseJson) = (t \ f) append (t \ g)
  }

  case class ComposableNotFilter(f: Filter) extends Filter {
    override def filter(t: TraverseJson) = t diff (t \ f)
  }

  case class ComposableNestFilter(key: String, g: Filter) extends Filter {
    override def filter(t: TraverseJson) = t.filter(n => (n \ key) \ g != Empty)
  }

  implicit class ToTraverseJson(node: JsonNode) {
    def \(name: String): TraverseJson = TraverseJson(Option(node.get(name)))
    def \(index: Int): TraverseJson = TraverseJson(Option(node.get(index)))
  }

  implicit class ToSimpleFilter(key: String) {
    def `=`(value: String) = SimpleFilter(key, value)
    def \ (g: Filter) = ComposableNestFilter(key, g)
  }

  implicit class ToComposableFilter(f: Filter) {
    def && (g: Filter) = ComposableAndFilter(f, g)
    def || (g: Filter) = ComposableOrFilter(f, g)
    def unary_! = ComposableNotFilter(f)
  }

  sealed trait Filter {
    def filter(t: TraverseJson): TraverseJson
  }
}
