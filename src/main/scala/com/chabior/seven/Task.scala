package com.chabior.seven

case class Task(label: Char, duration: Int) {
   var left = duration
   def work: Unit = {
     left = left - 1
   }

   def finished: Boolean = {
     left == 0
   }
}
