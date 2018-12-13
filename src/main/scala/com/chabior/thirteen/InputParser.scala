package com.chabior.thirteen

import scala.io.Source

object InputParser {
  def parse: Track = {
    var x = 0
    val track = new Track
    Source.fromResource("thirteen").getLines.foreach(line => {
      var y = 0
      line.grouped(1).foreach(sign => {
        val part = TrackPart.create(sign)
        track.addPart(part, x, y)
        y = y + 1
      })
      x = x + 1
    })

    track
  }

  def main(args: Array[String]): Unit = {
    var track = parse
    track.print
    for (i <- 1 to 20) {
      println(i)
      track = track.tick
      track.print
    }
  }
}
