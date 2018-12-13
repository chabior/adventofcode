package com.chabior.thirteen

class Track {
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

  def print: Unit = {
    val xs = parts.keys.toList.sorted
    for (x <- xs) {
      val ys =parts(x).keys.toList.sorted
      var line = ""
      for (y <- ys) {
        line = line + parts(x)(y).sign
      }
      println(line)
    }
  }

  def tick: Track = {
    val track = new Track
    val maxX = parts.keys.toList.max
    val maxY = parts.toList.flatMap(x => x._2.keys).max

    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        parts(x).get(y) match {
          case None =>
          case Some(part) => {
            if (part.isMoving) {
              val coord = part.move(x, y)
              val sign = parts(coord._1)(coord._2)
              track.addPart(part.switchDirection(sign.sign), coord._1, coord._2)
              track.addPart(part.previousDirection, x, y)
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
  def sign:String
  def isMoving: Boolean = false
  def move(x: Int, y: Int): (Int, Int) = (0, 0)
  def switchDirection(newPositionSign: String) :TrackPart = this
  def previousDirection: TrackPart = this
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

class Vehicle(s: String, lastDirection: Int = 2, previous: Option[String] = None) extends TrackPart {
  val directions = Map(0 -> "left", 1 -> "straight", 2 -> "right")

  override def sign: String = s

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
      case "-" => new Vehicle(s, lastDirection, Some(newPositionSign))
      case "|" => new Vehicle(s, lastDirection, Some(newPositionSign))
      case "\\" => {
        s match {
          case ">" => new Vehicle("v", lastDirection, Some(newPositionSign))
          case "v" => new Vehicle(">", lastDirection, Some(newPositionSign))
          case "^" => new Vehicle("<", lastDirection, Some(newPositionSign))
          case "<" => new Vehicle("^", lastDirection, Some(newPositionSign))
        }
      }
      case "/" => {
        s match {
          case "^" => new Vehicle(">", lastDirection, Some(newPositionSign))
          case ">" => new Vehicle("^", lastDirection, Some(newPositionSign))
          case "v" => new Vehicle("<", lastDirection, Some(newPositionSign))
          case "<" => new Vehicle("v", lastDirection, Some(newPositionSign))
        }
      }
      case "+" => {
        val d = (lastDirection + 1) % 3
        directions(d) match {
          case "left" => {
            s match {
              case ">" => new Vehicle("^", d, Some("+"))
              case "<" => new Vehicle("v", d, Some("+"))
              case "^" => new Vehicle("<", d, Some("+"))
              case "v" => new Vehicle(">", d, Some("+"))
            }
          }
          case "right" => {
            s match {
              case ">" => new Vehicle("v", d, Some("+"))
              case "<" => new Vehicle("^", d, Some("+"))
              case "^" => new Vehicle(">", d, Some("+"))
              case "v" => new Vehicle("<", d, Some("+"))
            }
          }
          case "straight" => new Vehicle(s, d, Some("+"))
          case _ => throw new RuntimeException("Cant move!")
        }
      }
      case ">" => new Vehicle(s, lastDirection, previous)
      case "<" => new Vehicle(s, lastDirection, previous)
      case "^" => new Vehicle(s, lastDirection, previous)
      case "v" => new Vehicle(s, lastDirection, previous)
      case x => {
        throw new RuntimeException(s"Out of track $x")
      }
    }
  }

  override def previousDirection: TrackPart = {
    if (previous.isDefined) {
      TrackPart.create(previous.get)
    } else {
      s match {
        case ">" => new Horizontal
        case "<" => new Horizontal
        case "^" => new Vertical
        case "v" => new Vertical
        case _ => throw new RuntimeException("Not a vehicle")
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
