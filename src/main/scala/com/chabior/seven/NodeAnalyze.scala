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

  def getPathMulti(graph: Map[Char, List[Char]], workers: List[Int]): Int = {
    var step = 0
    def r(graph: Map[Char, List[Char]], path: Map[Char, Int], step: Int): Map[Char, Int] = {
      if (graph.isEmpty) path else {
        val parents = graph.filter(x => x._2.isEmpty).toList.sortBy(_._1).take(workers.length)
        println(parents)
        var subgraph = graph
        for (parent <- parents) {
          subgraph = subgraph - parent._1
        }

        var subpath = path
        var d = step
        for (parent <- parents) {
          var ok = subpath
          if (ok.get(parent._1) match {
            case Some(_) => false
            case None => true
          }) {
            ok = ok + (parent._1 -> d)
          }
          d = gefNodeDur(parent._1, d)
          val s = r(subgraph.map(x => x._1 -> x._2.filterNot(_ == parent._1)), ok, d)
          for ((k, c) <- s) {
            subpath = subpath.get(k) match {
              case Some(_) => subpath
              case None => subpath + (k -> c)
            }
          }
          for ((k, c) <- s) {
            subgraph = subgraph.map(x => x._1 -> x._2.filterNot(_ == k))
          }
        }
        subpath
      }
    }
    val data = r(graph, Map.empty[Char, Int], step)
    data.toList.sortBy(_._2).map(_._2).max - 1
  }

  def gefNodeDur(node: Char, add: Int): Int = {
    val a = 'A' to 'Z'
    60 + a.indexOf(node) + 1 + add
  }
}
