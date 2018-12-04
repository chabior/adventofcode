package com.chabior.three

case class Fabric(clientNumber: Int, left:Int, top: Int, width: Int, height: Int) {
    var coordinates: Map[Int, List[Int]] = Map.empty
    for (x <- left until (left + width)) {
      for (y <- top until top + height) {
        coordinates = coordinates + (x -> (coordinates.getOrElse(x, List.empty[Int]) :+ y))
      }
    }

  var coordinatesList: List[(Int, Int)] = List.empty
  for (x <- left until (left + width)) {
    for (y <- top until top + height) {
      coordinatesList = coordinatesList :+ (x, y)
    }
  }

  def isOverlapping(compare: Fabric): Boolean = {
    for ((x, ys) <- coordinates) {
      val found = compare.coordinates.get(x) match {
        case Some(cys) => {
          ys.find(y => cys.find(cy => y == cy) match {
            case Some(_) => true
            case None => false
          }) match {
            case Some(_) => true
            case None => false
          }
        }
        case None => false
      }
      if (found) {
        return true
      }
    }

    false
  }

}
