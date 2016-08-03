package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode

import scala.language.{implicitConversions, postfixOps}
import scala.collection.JavaConverters._

/**
 * Created by trent ahrens on 5/15/15.
 */

trait PathSegmentImplicits extends JsTraverseImplicits {
  implicit class KeySegmentTraverse(traverse: JsTraverse) {
    def \(key: String): JsTraverse = traverse.withPathSegment(KeySegment(key))
  }

  implicit class KeySegmentJsonNode(node: JsonNode) {
    def \(key: String): JsTraverse = KeySegmentTraverse(node) \ key
  }

  implicit class FilteredSegmentTraverse(traverse: JsTraverse) {
    def \(filter: JsFilter): JsTraverse = traverse.withPathSegment(FilteredSegment(filter))
  }

  implicit class FilteredSegmentJsonNode(node: JsonNode) {
    def \(filter: JsFilter): JsTraverse = FilteredSegmentTraverse(node) \ filter
  }

  implicit class IndexSegmentTraverse(traverse: JsTraverse) {
    def \(index: Int): JsTraverse = traverse.withPathSegment(IndexSegment(index))
  }

  implicit class IndexSegmentJsonNode(node: JsonNode) {
    def \(index: Int): JsTraverse = IndexSegmentTraverse(node) \ index
  }

  implicit class FanOutSegmentTraverse(traverse: JsTraverse) {
    def explode : JsTraverse = traverse.withPathSegment(FanOutSegment)
  }

  implicit class FanOutSegmentJsonNode(node: JsonNode) {
    def explode : JsTraverse = FanOutSegmentTraverse(node).explode
  }
}

trait PathSegment {
  def apply(nodes: Seq[JsonNode]): Seq[JsonNode]
}

case class KeySegment(key: String) extends PathSegment {
  def apply(nodes: Seq[JsonNode]): Seq[JsonNode] =
    nodes match {
      case Seq(h, tail@_*) => Seq(Option(h.get(key))).flatten ++ apply(tail)
      case _ => Nil
    }

  override def toString: String = key
}

case class IndexSegment(index: Int) extends PathSegment {
  def apply(nodes: Seq[JsonNode]): Seq[JsonNode] =
    nodes.filter(_.isArray).flatMap(n => Option(n.get(index)))

  override def toString: String = index.toString
}

case class FilteredSegment(filter: JsFilter) extends PathSegment {
  def apply(nodes: Seq[JsonNode]): Seq[JsonNode] = nodes.filter(filter(_))

  override def toString: String = s"$filter"
}

case object FanOutSegment extends PathSegment {
  def apply(nodes: Seq[JsonNode]): Seq[JsonNode] = nodes.filter(!_.isArray) ++ nodes.filter(_.isArray).flatMap(_.elements().asScala)

  override def toString: String = "explode"
}

object JsPath extends JsPath(Seq.empty)

case class JsPath(segments: Seq[PathSegment] = Seq.empty) {
  def \(key: String) = JsPath(segments :+ KeySegment(key))
  
  def \(index: Int) = JsPath(segments :+ IndexSegment(index))

  def \(filter: JsFilter) = JsPath(segments :+ FilteredSegment(filter))

  def explode = JsPath(segments :+ FanOutSegment)

  def +(segment: PathSegment): JsPath = JsPath(segments :+ segment)

  def apply(node: JsonNode): JsTraverse = segments.foldLeft(JsTraverse(node))((t,s) => t withPathSegment s)

  override def toString: String = segments.map(_.toString).mkString(" \\ ")
}
