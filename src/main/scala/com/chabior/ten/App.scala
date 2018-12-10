package com.chabior.ten

object App {
  def main(args: Array[String]): Unit = {
    val surface = InputParser.parse
//    var min = 99999999L
//    var minI = 0
//    var i = 0
//    while (i > -1) {
//      val d =surface.distance(i)
//      if (d < min) {
//        min = d
//        minI = i
//        i = i + 1
//      } else {
//        println(i)
//        surface.print(i - 1)
//        i = -1
//      }
//    }
    for (i <- (10570 to 10580)) {
      surface.print(i)
    }
  }
}
