package com.chabior.fourteen

import scala.io.Source

object InputParser {
  def main(args: Array[String]): Unit = {
    parse
  }

  def parse: Unit = {
    var receipies = "37"
    val a = 513401
    var first = 0
    var second = 1

    def tick(r: String): String = {
      val s = (r.charAt(first).toString.toInt + r.charAt(second).toString.toInt).toString
      val n = r + s
      val len = n.length
      first = (n.charAt(first).toString.toInt + 1 + first) % len
      second = (n.charAt(second).toString.toInt + 1 + second) % len
      n
    }

    var i = 0
    while(receipies.takeRight(6).toInt != a && receipies.takeRight(7).take(6).toInt != a) {
      receipies = tick(receipies)
      println(receipies.length - 6)
    }

    println(receipies.length - 6)
  }
}

