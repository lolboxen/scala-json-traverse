package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode

import scala.collection.JavaConversions._
import scala.language.implicitConversions

/**
 * Created by trent ahrens on 5/15/15.
 */

trait JsTraverseImplicits {
  implicit def toJsTraverse(node: JsonNode): JsTraverse = JsTraverse(node)
}

object JsTraverse {
  val empty = JsTraverse(List.empty, JsPath(Nil))

  def apply(node: JsonNode): JsTraverse = apply(Option(node))

  def apply(node: Option[JsonNode]): JsTraverse = node match {
    case Some(n) => JsTraverse(Seq(n), JsPath(Nil))
    case None => empty
  }
}

case class JsTraverse(nodes: Seq[JsonNode], path: JsPath) {
  def length: Int = nodes.length

  def diff(t: JsTraverse): JsTraverse = JsTraverse(nodes.diff(t.nodes), path)

  def and(t: JsTraverse): JsTraverse = JsTraverse(nodes.filter(t.nodes.contains(_)), path)

  def append(t: JsTraverse): JsTraverse = JsTraverse(nodes ++ t.nodes, path)

  def withPathSegment(segment: PathSegment): JsTraverse = JsTraverse(segment(nodes), path.+(segment))

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

  def allAs[A](implicit reads: Reads[A]): Seq[A] = {
    def loop(result: Seq[A], seq: Seq[JsonNode]): Seq[A] =
      seq match {
        case head :: tails =>
          reads.reads(head) match {
            case JsSuccess(value) => loop(result :+ value, tails)
            case JsError(reason) => throw new Exception(reason)
          }
        case Nil => result
      }

    loop(Nil, nodes.toList)
  }

  def allAsOpt[A](implicit reads: Reads[A]): Option[Seq[A]] = {
    def loop(result: Seq[A], seq: Seq[JsonNode]): Option[Seq[A]] =
      seq match {
        case head :: tails =>
          reads.reads(head) match {
            case JsSuccess(value) => loop(result :+ value, tails)
            case JsError(reason) => None
          }
        case Nil => Some(result)
      }

    loop(Nil, nodes.toList)
  }

  def validate[A](implicit reads: Reads[A]): JsResult[A] = nodes.headOption match {
    case Some(n) => reads.reads(n)
    case _ => JsError("path not found")
  }
}
