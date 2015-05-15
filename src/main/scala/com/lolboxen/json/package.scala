package com.lolboxen

import com.fasterxml.jackson.databind.JsonNode

import scala.language.implicitConversions

/**
 * Created by trent ahrens on 5/15/15.
 */
package object json {
  implicit def toJsTraverse(node: JsonNode): JsTraverse = JsTraverse(node)
}
