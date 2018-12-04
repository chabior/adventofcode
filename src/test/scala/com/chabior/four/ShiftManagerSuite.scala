package com.chabior.four

import org.joda.time.format.DateTimeFormat
import org.scalatest.FlatSpec

class ShiftManagerSuite extends FlatSpec {
  it should "find longest sleep" in {

    val pattern = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
    val firstShift = Shift(10, pattern.parseDateTime("1518-11-01 00:00"))
    firstShift.addSleep((pattern.parseDateTime("1518-11-01 00:05"), pattern.parseDateTime("1518-11-01 00:25")))
    firstShift.addSleep((pattern.parseDateTime("1518-11-01 00:30"), pattern.parseDateTime("1518-11-01 00:55")))

    val secondShift = Shift(99, pattern.parseDateTime("1518-11-01 23:58"))
    secondShift.addSleep((pattern.parseDateTime("1518-11-02 00:40"), pattern.parseDateTime("1518-11-02 00:50")))

    val third = Shift(10, pattern.parseDateTime("1518-11-03 00:05"))
    third.addSleep((pattern.parseDateTime("1518-11-03 00:24"), pattern.parseDateTime("1518-11-03 00:29")))

    val fourth = Shift(99, pattern.parseDateTime("1518-11-04 00:02"))
    fourth.addSleep((pattern.parseDateTime("1518-11-04 00:36"), pattern.parseDateTime("1518-11-04 00:46")))

    val fifth = Shift(99, pattern.parseDateTime("1518-11-05 00:03"))
    fifth.addSleep((pattern.parseDateTime("1518-11-05 00:45"), pattern.parseDateTime("1518-11-05 00:55")))


    val shifts = List(
      firstShift, secondShift, third, fourth, fifth
    )
    assert(ShiftManager.findLongestSleep(shifts) == 240)
  }

  it should "find longest sleep from file source" in {
    assert(ShiftManager.findLongestSleep(InputParser.parse("security_journal_test")) == 240)
    assert(ShiftManager.findLongestSleep(InputParser.parse("security_journal")) == 12169)
  }

  it should "find most sleep minute" in {
    assert(ShiftManager.findMostSleepMinute(InputParser.parse("security_journal")) == 16164)
    assert(ShiftManager.findMostSleepMinute(InputParser.parse("security_journal_test")) == 4455)
  }
}
