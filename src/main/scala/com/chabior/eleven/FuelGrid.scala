package com.chabior.eleven

class FuelGrid(val serialNumber: Int) {
  var grid = Map.empty[Int, Map[Int, Int]]
  for (y <- 1 to 300) {
    var xs = Map.empty[Int, Int]
    for (x <- 1 to 300) {
      val rackId = y + 10
      val fuelLevel = (rackId * x + serialNumber) * rackId
      val c = fuelLevel.toString.reverse.charAt(2).toString.toInt
      val level = c - 5
      xs = xs + (x -> level)
    }
    grid = grid + (y -> xs)
  }

  def findMaxLevel: (Int, Int) = {
    var max = -99999
    var maxTuple = (0, 0)
    for (x <- 1 to 298) {
      for (y <- 1 to 298) {
        val s =  sum(x, y)
        if (s > max) {
          max = s
          maxTuple = (x, y)
        }
      }
    }
    maxTuple
  }

  def sum(address: (Int, Int)): Int = {
    grid(address._1)(address._2) + grid(address._1 + 1)(address._2) + grid(address._1 + 2)(address._2) +
      grid(address._1)(address._2 + 1) + grid(address._1 + 1)(address._2 +1 ) + grid(address._1 + 2)(address._2 + 1) +
      grid(address._1)(address._2 + 2) + grid(address._1 + 1)(address._2 +2) + grid(address._1 + 2)(address._2 + 2)
  }

  def sumSize(address: (Int, Int), size: Int) : Int = {
    var sum = 0
    for (sx <- 0 until size) {
      for (sy <- 0 until size) {
        sum = sum + grid(address._1 + sx)(address._2 + sy)
      }
    }
    sum
  }

  def maxWithSize: (Int, Int, Int) = {
    var max = -999999
    var maxTuple = (0, 0, 0)
    for (size <- 1 to 300) {
      println(size)
      for (x <- 1 to (300 - size + 1)) {
        for (y <- 1 to (300 - size + 1)) {
          val s = sumSize((x, y), size)
          if (s > max) {
            max = s
            maxTuple = (x, y, size)
            println(maxTuple)
          }
        }
      }
    }
    maxTuple
  }

}
