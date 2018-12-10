package com.chabior.ten

import scala.io.Source

object InputParser {
  def parse:Surface = {
    val points = Source.fromResource("ten").getLines().map(line => {
      val pattern = """position=<([\- 0-9]+), ([\- 0-9]+)> velocity=<([\- 0-9]+), ([\- 0-9]+)>""".r
      val pattern(x, y, vx, vy) = line

      Point((x.trim.toInt, y.trim.toInt), (vx.trim.toInt, vy.trim.toInt))
    }).toList
    new Surface(points)
  }
}
