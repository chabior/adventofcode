package com.chabior.six

class Area(x: Int, y: Int) {
  var grid : Map[Int, Map[Int, (Int, Int)]] = Map.empty
  for (xs <- 0 to x) {
    var sub = Map.empty[Int, (Int, Int)]
    for (ys <- 0 to y) {
      sub = sub + (ys -> (0, 999999))
    }
    grid = grid + (xs -> sub)
  }

  def distances(point: DataPoint): Unit = {
    for (x <- grid.keys) {
      for (y <- grid(x).keys) {
        val m = manhattan(x, y, point.x, point.y)
        var tempY: (Int, Int) = grid.get(x) match {
          case Some(z) => z.get(y) match {
            case Some(k) => k
            case None => throw new RuntimeException
          }
          case None => throw new RuntimeException
        }

        if (tempY._2 > m) {
          tempY = (point.label, m)
        } else {
          if (tempY._2 == m) {
            tempY = (0, m)
          }
        }

        var test = grid(x)
        test =  test + (y -> tempY)
        grid = grid + (x -> test)
      }
    }
  }

  def total(dataPoint: DataPoint): Unit = {

  }

  def manhattan(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    Math.abs(x1 - x2) + Math.abs(y1 - y2)
  }
}
