package com.chabior.thirteen

class Track {
  var withOutVehicles: Track = _
  var parts = Map.empty[Int, Map[Int, TrackPart]]

  def addPart(part:TrackPart, x: Int, y: Int): Unit = {
    val xs = parts.getOrElse(x, Map.empty[Int, TrackPart])
    xs.get(y) match {
      case None => parts = parts + (x -> (xs + (y -> part)))
      case Some(p) => {
        if (!p.isMoving) {
          parts = parts + (x -> (xs + (y -> part)))
        } else {
          if (part.isMoving) {
            throw new RuntimeException(s"Collision ($y, $x)")
          }
        }
      }
    }
  }

  def removeVehicles: Track = {
    val track = new Track
    for ((x, xs) <- parts) {
      for ((y, part) <- xs) {
        if (part.isMoving) {
          part.sign match {
            case ">" => track.addPart(new Horizontal, x, y)
            case "<" => track.addPart(new Horizontal, x, y)
            case "^" => track.addPart(new Vertical, x, y)
            case "v" => track.addPart(new Vertical, x, y)
          }
        } else {
          track.addPart(part, x, y)
        }
      }
    }
    track
  }

  def print: Unit = {
    val xs = parts.keys.toList.sorted
    for (x <- xs) {
      val ys = parts(x).keys.toList.sorted
      var line = ""
      for (y <- ys) {
        line = line + parts(x)(y).print
      }
      println(line)
    }
  }

  def tick: Track = {
    val track = new Track
    track.withOutVehicles = withOutVehicles
    val maxX = parts.keys.toList.max
    val maxY = parts.toList.flatMap(x => x._2.keys).max

    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        parts(x).get(y) match {
          case None =>
          case Some(part) => {
            if (part.isMoving) {
              val coord = part.move(x, y)
              val sign = withOutVehicles.parts(coord._1)(coord._2)
              track.addPart(part.switchDirection(sign.sign), coord._1, coord._2)
              track.addPart(withOutVehicles.parts(x)(y), x, y)
            } else {
              track.addPart(part, x, y)
            }
          }
        }
      }
    }

    track
  }
}

trait TrackPart {
  def print: String = sign
  def sign:String
  def isMoving: Boolean = false
  def move(x: Int, y: Int): (Int, Int) = (0, 0)
  def switchDirection(newPositionSign: String) :TrackPart = this
}
class Intersection extends TrackPart {
  override def sign: String = "+"
}
class Horizontal extends TrackPart {
  override def sign: String = "-"
}
class Vertical extends TrackPart{
  override def sign: String = "|"
}
class Slash extends TrackPart {
  override def sign: String = "/"
}
class BackSlash extends TrackPart {
  override def sign: String = "\\"
}

class Vehicle(s: String, lastDirection: Int = 2) extends TrackPart {
  val directions = Map(0 -> "left", 1 -> "straight", 2 -> "right")

  override def sign: String = s

  override def print: String = Console.RED + s + Console.WHITE

  override def isMoving: Boolean = true

  override def move(x: Int, y: Int): (Int, Int) = {
    s match {
      case ">" => (x, y + 1)
      case "<" => (x, y - 1)
      case "^" => (x - 1, y)
      case "v" => (x + 1, y)
    }
  }

  override def switchDirection(newPositionSign: String): TrackPart = {
    newPositionSign match {
      case "-" => new Vehicle(s, lastDirection)
      case "|" => new Vehicle(s, lastDirection)
      case "\\" => {
        s match {
          case ">" => new Vehicle("v", lastDirection)
          case "v" => new Vehicle(">", lastDirection)
          case "^" => new Vehicle("<", lastDirection)
          case "<" => new Vehicle("^", lastDirection)
        }
      }
      case "/" => {
        s match {
          case "^" => new Vehicle(">", lastDirection)
          case ">" => new Vehicle("^", lastDirection)
          case "v" => new Vehicle("<", lastDirection)
          case "<" => new Vehicle("v", lastDirection)
        }
      }
      case "+" => {
        val d = (lastDirection + 1) % 3
        directions(d) match {
          case "left" => {
            s match {
              case ">" => new Vehicle("^", d)
              case "<" => new Vehicle("v", d)
              case "^" => new Vehicle("<", d)
              case "v" => new Vehicle(">", d)
            }
          }
          case "right" => {
            s match {
              case ">" => new Vehicle("v", d)
              case "<" => new Vehicle("^", d)
              case "^" => new Vehicle(">", d)
              case "v" => new Vehicle("<", d)
            }
          }
          case "straight" => new Vehicle(s, d)
          case _ => throw new RuntimeException("Cant move!")
        }
      }
      case x => {
        throw new RuntimeException(s"Out of track $x")
      }
    }
  }
}

class Space extends TrackPart {
  override def sign: String = " "
}

object TrackPart {
  def create(sign: String): TrackPart = {
    sign match {
      case "+" => new Intersection
      case "-" => new Horizontal
      case "|" => new Vertical
      case "/" => new Slash
      case "\\" => new BackSlash
      case ">" => new Vehicle(">")
      case "<" => new Vehicle("<")
      case "^" => new Vehicle("^")
      case "v" => new Vehicle("v")
      case " " => new Space
      case x => throw new RuntimeException(s"There is no track part for sign $x")
    }
  }
}
