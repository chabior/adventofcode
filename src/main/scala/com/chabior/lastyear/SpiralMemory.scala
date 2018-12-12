package com.chabior.lastyear

object SpiralMemory {
  def main(args: Array[String]): Unit = {
    val mem = new SpiralMemory
    for (i <- 1 to 1000000) {
      mem.add
    }
    println(mem.data)
  }
}

class SpiralMemory {
  var data = Map.empty[(Int, Int), Int]
  var currentAddress = (0, 0)
  var currDirection = 0
  val directions = Map(0 -> (1, 0), 1 -> (0, 1), 2 -> (-1, 0), 3 -> (0, -1))

  def add(): Unit = {
    if (data.isEmpty) {
      data = data + ((0, 0) -> 1)
      currDirection = 0
      currentAddress = (0, 0)
    } else {
      val add = getCurrAddress
      data.get(add) match {
        case None => {
          data = data + (add -> getVal(add))
          currentAddress = add
          moveToNextDir
        }
        case Some(x) => {
          moveToPrevDir
          val add = getCurrAddress
          data = data + (add -> getVal(add))
          currentAddress = add
          moveToNextDir
        }
      }
    }
  }

  def getCurrAddress: (Int, Int) = {
    val dir = getCurrDir
    (currentAddress._1 + dir._1, currentAddress._2 + dir._2 )
  }

  def getCurrDir: (Int, Int) = {
    directions.get(currDirection) match {
      case None => throw new RuntimeException
      case Some(x) => x
    }
  }

  def moveToNextDir: Unit = {
    currDirection = (currDirection + 1) % 4
  }
  def moveToPrevDir: Unit = {
    if (currDirection - 1 < 0) {
      currDirection = 3
    } else {
      currDirection = currDirection - 1
    }
  }

  def getDistance: Int = {
    Math.abs(currentAddress._1) + Math.abs(currentAddress._2)
  }

  def getData(address: (Int, Int)): Int = {
    data.getOrElse(address, 0)
  }

  def getVal(add: (Int, Int)): Int = {
    val d = List(
      (-1, -1),(-1, 0),(-1, 1),
      (0, -1),(0, 1),
      (1, -1),(1, 0),(1, 1),
    ).map(x => getData((x._1 + add._1, x._2 + add._2))).sum
    if (d > 265149) {
      throw new RuntimeException(s"LARGER: $d")
    }
    d
  }
}
