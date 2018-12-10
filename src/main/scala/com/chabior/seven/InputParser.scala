package com.chabior.seven

import scala.io.Source

object InputParser {
  def parse: Map[Char, List[Char]] = {
    var graph = Map.empty[Char, List[Char]]
    Source.fromResource("steps").getLines.foreach(line => {
      val pattern = "Step ([A-Z]{1}) must be finished before step ([A-Z]{1}) can begin.".r
      val pattern(parent, child) = line
      val t = graph.getOrElse(child.head, List.empty[Char]) :+ parent.head
      graph = graph + (child.head -> t)
      if (graph.get(parent.head) match {
        case None => true
        case Some(_) => false
      }) {
        graph = graph + (parent.head -> List.empty[Char])
      }
    })

    graph
  }

  def parsePartTwo: Unit = {
    Source.fromResource("steps_test").getLines.foreach(line => {
      val pattern = "Step ([A-Z]{1}) must be finished before step ([A-Z]{1}) can begin.".r
      val pattern(parent, child) = line

    })
  }
}
