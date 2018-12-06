package com.chabior.lastyear

import scala.io.Source

object Captcha {
  def main(args: Array[String]): Unit = {
    val captcha = Source.fromResource("lastyear/captcha").getLines.map(_.sliding(1)).flatMap(x => x.map(z => z.toInt)).toList
    println(Captcha.findSum(captcha))
  }

  def findSum(input: List[Int]): Int = {
    input.indices.map(i => {
      val k = (i + input.length / 2) % input.length
      if (input(i) == input(k)) Option(input(i)) else None
    }).map {
      case Some(z:Int) => z
      case None => 0
    }.sum
  }
}
