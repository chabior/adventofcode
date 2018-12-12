package com.chabior.lastyear

import scala.io.Source

object Passphrase {
  def main(args: Array[String]): Unit = {
    val phrases = Source.fromResource("lastyear/passphrase").getLines.filter(x => {
      val splitted = x.split(' ')
      val g = splitted.toList.map(x => x.toList.sorted)

      g.distinct.length == g.length
    })

    println(phrases.length)
  }
}
