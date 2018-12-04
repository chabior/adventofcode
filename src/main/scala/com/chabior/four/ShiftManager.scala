package com.chabior.four

object ShiftManager {
  def findLongestSleep(shifts: List[Shift]): Int = {
    val guard = shifts.groupBy(shift => shift.guardId).map(x => (x._1, x._2.map(x => x.sleepDuration).sum)).toArray.sortBy(_._2).maxBy(_._2)

    val mostMinute = shifts.filter(x => x.guardId == guard._1).flatMap(x => x.sleepMinutes).groupBy(identity).toArray.sortBy(_._1).maxBy(_._2.length)._1
    guard._1 * mostMinute
  }

  def findMostSleepMinute(shifts: List[Shift]): Int = {
    val mostMinute = shifts.filter(x => x.sleeps.nonEmpty).groupBy(shift => shift.guardId).map(x => {
      val m = x._2.flatMap(z => z.sleepMinutes).groupBy(identity).toArray.sortBy(_._1).maxBy(_._2.length)
      (x._1, m._1, m._2.length)
    }).toArray.maxBy(_._3)
    mostMinute._1 * mostMinute._2
  }
}
