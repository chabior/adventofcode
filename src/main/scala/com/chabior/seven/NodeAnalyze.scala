package com.chabior.seven

object NodeAnalyze {
  def getPath(graph: Map[Char, List[Char]]): List[Char] = {
    def r(graph: Map[Char, List[Char]], path: List[Char]):List[Char] = {
      if (graph.isEmpty) path else {
        val parent = graph.toList.sortBy(_._1).find(x => x._2.isEmpty) match {
          case Some(z) => z._1
          case None => throw  new RuntimeException
        }
        println(graph.map(x => x._1 -> x._2.filterNot(_ == parent)) - parent)
        r(graph.map(x => x._1 -> x._2.filterNot(_ == parent)) - parent, path:+parent)
      }
    }
    r(graph, List.empty[Char])
  }
}
