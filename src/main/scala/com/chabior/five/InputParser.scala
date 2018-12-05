package com.chabior.five

import scala.io.Source

object InputParser {
  def parse:String = {
    Source.fromResource("polymer").getLines.mkString
  }
}
