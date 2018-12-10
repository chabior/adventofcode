package com.chabior.seven

object NodeAnalyze {
  def getPath(graph: Map[Char, List[Char]]): List[Char] = {
    def r(graph: Map[Char, List[Char]], path: List[Char]):List[Char] = {
      if (graph.isEmpty) path else {
        val parent = graph.toList.sortBy(_._1).find(x => x._2.isEmpty) match {
          case Some(z) => z._1
          case None => throw  new RuntimeException
        }

        r(graph.map(x => x._1 -> x._2.filterNot(_ == parent)) - parent, path:+parent)
      }
    }
    r(graph, List.empty[Char])
  }

  def getPathMulti(graph: Map[Char, List[Char]], workers: List[Worker]): Int = {
    var subgraph = graph
    var duration = 0
    var path = ""

    def isWorking: Boolean = {
      workers.find(x => !x.empty) match {
        case None => false
        case Some(_) => true
      }
    }

    while (subgraph.nonEmpty || isWorking) {
      for (worker <- workers) {
        if (!worker.empty) {
          worker.work match {
            case None =>
            case Some(finished) => {
              subgraph = subgraph.map(x => x._1 -> x._2.filterNot(_ == finished))
              path = path + finished
            }
          }

        }
      }
      val parents = subgraph.toList.filter(x => x._2.isEmpty)

      for (parent <- parents) {
        workers.find(x => x.empty) match {
          case None =>
          case Some(worker) => {
            subgraph = subgraph - parent._1
            worker.addTask(Task(parent._1, gefNodeDur(parent._1)))
          }
        }
      }

      duration = duration + 1
    }
    println(path)
    duration - 1
  }

  def gefNodeDur(node: Char): Int = {
    val a = 'A' to 'Z'
    60 + a.indexOf(node) + 1
  }
}
