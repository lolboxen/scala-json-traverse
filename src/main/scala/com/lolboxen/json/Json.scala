package com.lolboxen.json

import com.fasterxml.jackson.databind.ObjectMapper

/**
 * Created by trent ahrens on 4/23/15.
 */
object Json {
  val mapper = new ObjectMapper()

  def parse(s: String) = mapper.readTree(s)
}
