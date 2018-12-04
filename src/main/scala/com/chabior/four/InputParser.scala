package com.chabior.four

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.io.Source

object InputParser {
    import Joda._
    def parse(filename: String):List[Shift] = {
      var i = 0
      val parsed = Source.fromResource(filename).getLines.map(line => {
        val pattern = """\[(.*)\] (.*)""".r
        val pattern(dateString, infoStr) = line

        val date = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm").parseDateTime(dateString)
        (date, infoStr)
      }).toArray.sortBy(_._1).toList.groupBy(x => {
        "Guard #[0-9]+ begins shift".r.findFirstIn(x._2) match {
          case Some(_) => {
            i = i + 1
            i
          }
          case None => i
        }
      })

      var shifts: List[Shift] = List.empty
      for (shiftNumber <- 1 to parsed.keys.toList.length) {
        parsed get shiftNumber match {
          case Some(x) => {
            val info = x.head
            val guardPattern = "Guard #([0-9]+) begins shift".r
            val guardPattern(guardId) = info._2
            val shift = Shift(guardId.toInt, info._1)

            x.tail.grouped(2).toArray.sortBy(x => x.head._1).toList.map(x => (x.head._1, x.tail.head._1)).foreach(sleep => shift.addSleep(sleep))
            shifts = shifts :+ shift
          }
          case None =>
        }
      }
      shifts
    }
}

object Joda {
  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
}