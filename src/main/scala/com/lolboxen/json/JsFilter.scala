package com.lolboxen.json

import com.fasterxml.jackson.databind.JsonNode

/**
 * Created by trent ahrens on 5/15/15.
 */

trait JsFilterImplicits {
  implicit class ToSimpleFilter(key: String) {
    def `=`(value: String) = SimpleFilter(key, value)
    def \ (g: JsFilter) = ComposableNestFilter(key, g)
  }

  implicit class ToComposableFilter(f: JsFilter) {
    def && (g: JsFilter) = ComposableAndFilter(f, g)
    def || (g: JsFilter) = ComposableOrFilter(f, g)
    def unary_! = ComposableNotFilter(f)
  }
}

sealed trait JsFilter {
  def apply(node: JsonNode): Boolean
}

case class SimpleFilter(key: String, value: String) extends JsFilter with PathSegmentImplicits {
  override def apply(node: JsonNode): Boolean = (node \ key).asOpt[String].contains(value)
  override def toString: String = s"""("$key" `=` "$value")"""
}

case class ComposableAndFilter(f: JsFilter, g: JsFilter) extends JsFilter {
  override def apply(node: JsonNode): Boolean = f(node) && g(node)
  override def toString: String = s"""($f && $g)"""
}

case class ComposableOrFilter(f: JsFilter, g: JsFilter) extends JsFilter {
  override def apply(node: JsonNode): Boolean = f(node) || g(node)
  override def toString: String = s"""($f || $g)"""
}

case class ComposableNotFilter(f: JsFilter) extends JsFilter {
  override def apply(node: JsonNode): Boolean = !f(node)
  override def toString: String = s"""!$f"""
}

case class ComposableNestFilter(key: String, g: JsFilter) extends JsFilter {
  override def apply(node: JsonNode): Boolean = g(node.get(key))
  override def toString: String = s"""("$key" \\ $g)"""
}
