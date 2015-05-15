package com.lolboxen.json

import scala.language.postfixOps

/**
 * Created by trent ahrens on 5/15/15.
 */

trait PathSegment {
  def apply(traverse: JsTraverse): JsTraverse
}

case class KeySegment(name: String) extends PathSegment {
  def apply(traverse: JsTraverse): JsTraverse = traverse \ name
}

case class IndexSegment(index: Int) extends PathSegment {
  def apply(traverse: JsTraverse): JsTraverse = traverse \ index
}

case class FilteredSegment(filter: JsFilter) extends PathSegment {
  def apply(traverse: JsTraverse): JsTraverse = traverse \ filter
}

case object FanOutSegment extends PathSegment {
  def apply(traverse: JsTraverse): JsTraverse = traverse *
}

object JsPath extends JsPath(Seq.empty)

case class JsPath(segments: Seq[PathSegment] = Seq.empty) {
  def \(key: String) = JsPath(segments :+ KeySegment(key))
  
  def \(index: Int) = JsPath(segments :+ IndexSegment(index))

  def \(filter: JsFilter) = JsPath(segments :+ FilteredSegment(filter))

  def * = JsPath(segments :+ FanOutSegment)

  def apply(traverse: JsTraverse): JsTraverse = segments.foldLeft(traverse)((t,p) => p(t))
}
