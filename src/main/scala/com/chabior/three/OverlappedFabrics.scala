package com.chabior.three

object OverlappedFabrics {
  def count(fabrics: List[Fabric]): Int = {
    var mainFabric: Map[Int, List[Int]] = Map.empty[Int, List[Int]]
    for (fabric <- fabrics) {
      for (coordinate <- fabric.coordinatesList) {
        mainFabric = mainFabric + (coordinate._1 -> (mainFabric.getOrElse(coordinate._1, List.empty[Int]) :+ coordinate._2))
      }
    }
    mainFabric.map(tuple => tuple._2.groupBy(identity).collect { case (x, List(_,_,_*)) => x }.toList.length).sum
  }

  def findNot(fabrics: List[Fabric]): Int = {
    fabrics.find(
      fabric => fabrics.find(
        compare => fabric.clientNumber != compare.clientNumber && compare.isOverlapping(fabric)
      ) match {
        case Some(_) => false
        case None => true
      }
    ) match {
      case Some(x) => x.clientNumber
      case None => throw new RuntimeException("Fabric not found")
    }
  }
}
