package com.chabior.lastyear

import scala.io.Source

object Spreadsheet {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("lastyear/spreadsheet").getLines.map(x => x.split(",").map(_.toInt).toList).toList
    /**
      * 5 9 2 8
      * 9 4 7 3
      * 3 8 6 5
      */
    val test = List(List(5, 9, 2, 8), List(9, 4, 7, 3), List(3, 8, 6, 5))
    println(divideChecksum(input))
  }
  def checksum(input: List[List[Int]]): Int = {
    input.map(row => row.max - row.min).sum
  }

  def divideChecksum(input: List[List[Int]]): Int = {
    input.flatMap(row => {
      val combo = row.combinations(2).toList
      combo.filter(x => x.head % x.last == 0).map(x => x.head / x.last) ::: combo.filter(x => x.last % x.head == 0).map(x => x.last / x.head)
    }).sum
  }
}
