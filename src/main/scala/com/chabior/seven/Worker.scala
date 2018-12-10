package com.chabior.seven

class Worker {
  var currentTask: Option[Task] = None

  def addTask(task: Task): Unit = {
    currentTask = Some(task)
  }

  def empty: Boolean = currentTask match {
    case None => true
    case Some(_) => false
  }

  def work: Option[Char] = {
    currentTask match {
      case None => throw new RuntimeException
      case Some(x) => {
        x.work
        if (x.finished) {
          currentTask = None
          Some(x.label)
        } else {
          None
        }
      }
    }
  }
}
