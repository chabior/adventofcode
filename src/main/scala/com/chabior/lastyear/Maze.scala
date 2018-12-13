package com.chabior.lastyear

import scala.io.Source

object Maze {
  def main(args: Array[String]): Unit = {
    var map = Map.empty[Int, Int]
    var i =  0
    Source.fromResource("lastyear/maze").getLines.foreach(x => {
      map = map + (i -> x.toInt)
      i = i + 1
    })

    var d = 0
    var index = 0
    while (true) {
      map.get(index) match {
        case None => throw new RuntimeException(s"OUT: ${d - 1}")
        case Some(x) => {
          map = map + (index -> (x + 1))
          index = x + index
        }
      }
      d = d + 1
    }

  }
}

