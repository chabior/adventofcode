package com.chabior.six

import scala.io.Source

object InputParser {
  def parse: Unit = {
    var n = (0 to 99999).toList
    val dataPoints = Source.fromResource("grid").getLines.map(x => {
      val pattern = "([0-9]+), ([0-9]+)".r
      val pattern(xs, ys) = x
      val label = n.head
      n = n.tail

      DataPoint(label, xs.toInt, ys.toInt)
    }).toList

    val max = dataPoints.map(dataPoint => if (dataPoint.x > dataPoint.y) dataPoint.x else dataPoint.y).max

    val area = new Area(max, max)
    for (dataPoint <- dataPoints) {
      area.total(dataPoint)
    }


    var s = 0
    for ((x, ys) <- area.grid) {
      for ((y, total) <- ys) {
        if (total < 10000) {
          s = s + 1
        }
      }
    }
    println(s)
  }


}
