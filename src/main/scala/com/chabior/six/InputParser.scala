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
      area.distances(dataPoint)
    }

    var infinites = List.empty[Int]
    for ((x, ys) <- area.grid) {
      for ((y, point) <- ys) {
        if (x == 0 || y == 0 || x == max || y == max) {
          infinites = infinites :+ point._1
        }
      }
    }
    infinites = infinites.distinct

    var m = Map.empty[Int, Int]
    for ((x, ys) <- area.grid) {
      for ((y, point) <- ys) {
        val k = m.getOrElse(point._1, 0) + 1
        m = m + (point._1 -> k)
      }
    }

    var maxV = 0
    for ((label, size) <- m) {
      val isNotInfinite = infinites.find(_ == label) match {
        case Some(_) => false
        case None => true
      }

      if (isNotInfinite && size > maxV) {
        maxV = size
      }
    }

    println(maxV)
  }


}
