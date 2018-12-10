package com.chabior.ten

case class Point(startingPos: (Int, Int), velocity: (Int, Int)) {
  def getPosition(second: Int): (Int, Int) = {
    (startingPos._1 + (second * velocity._1), startingPos._2 + (second * velocity._2))
  }
}
