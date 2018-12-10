package com.chabior.ten

import java.io.{BufferedWriter, File, FileWriter}


class Surface(points: List[Point]) {

  def distance(second: Int): Long = {
    val x = points.map(i => i.getPosition(second)._1)
    val y = points.map(i => i.getPosition(second)._2)

    Math.abs(x.min - x.max) + Math.abs(y.min - y.max)
  }

  def print(second: Int): Unit = {
    val file = new File("/home/pchabierski/tests/test" + second)
    val bw = new BufferedWriter(new FileWriter(file))

    val s = getSurface(second)
    val surface = s._1

    val rangeY = s._4 to s._5
    for (x <-  s._2 to s._3) {
      surface.get(x) match {
        case Some(_) => {
          var line = ""
          for (y <- rangeY) {
            line = line + surface(x).getOrElse(y, ' ')
          }
          bw.write(line)
          bw.write("\n")
        }
        case None => {
          bw.write(rangeY.map(_ => ' ').mkString)
        }
      }
    }
    bw.close()
  }

  private def getSurface(second: Int) : (Map[Int, Map[Int, Char]], Int, Int, Int, Int) = {
    var surface = Map.empty[Int, Map[Int, Char]]

    for (point <- points) {
      val d = surface.getOrElse(point.getPosition(second)._2, Map.empty[Int, Char])
      surface = surface + (point.getPosition(second)._2 -> (d + (point.getPosition(second)._1 -> '#')))
    }

    val x = points.map(point => point.getPosition(second)._1)
    val minX = x.min
    val maxX = x.max

    val y = points.map(point => point.getPosition(second)._2)
    val minY = y.min
    val maxY = y.max

    (surface, minY, maxY, minX, maxX)
  }
}
