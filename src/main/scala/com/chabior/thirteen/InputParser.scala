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

    track.withOutVehicles = track.removeVehicles
    track
  }

  def main(args: Array[String]): Unit = {
    var track = parse
    for (i <- 1 to 2000) {
      track = track.tick
    }
  }
}
