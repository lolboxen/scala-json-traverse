package com.lolboxen.json

import com.lolboxen.json.JsTraverse._

/**
 * Created by trent ahrens on 5/15/15.
 */

object JsFilter {
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
  def apply(t: JsTraverse): JsTraverse
}

case class SimpleFilter(key: String, value: String) extends JsFilter {
  override def apply(t: JsTraverse) = t.filter(n => (n \ key).asOpt[String] == Some(value))
}

case class ComposableAndFilter(f: JsFilter, g: JsFilter) extends JsFilter {
  override def apply(t: JsTraverse) = (t \ f) and (t \ g)
}

case class ComposableOrFilter(f: JsFilter, g: JsFilter) extends JsFilter {
  override def apply(t: JsTraverse) = (t \ f) append (t \ g)
}

case class ComposableNotFilter(f: JsFilter) extends JsFilter {
  override def apply(t: JsTraverse) = t diff (t \ f)
}

case class ComposableNestFilter(key: String, g: JsFilter) extends JsFilter {
  override def apply(t: JsTraverse) = t.filter(n => (n \ key) \ g != empty)
}
