package com.chabior.thirteen

import scala.io.Source

object InputParser {
  def parse: Unit = {
    var x = 0
    var track = Map.empty[Int, Map[Int, Cart]]

    case class Cart(sign: String, c: Int = 2) {
      var d = Map(0 -> "l", 1 -> "s", 2 -> "r")
      def turn:Cart = {
        val c1 = (c + 1) % d.keys.toList.length
        d(c1) match {
          case "l" => {
            sign match {
              case ">" => Cart("^", c1)
              case "^" => Cart("<", c1)
              case "<" => Cart("v", c1)
              case "v" => Cart(">", c1)
            }
          }
          case "s" => Cart(sign, c1)
          case "r" => {
            sign match {
              case ">" => Cart("v", c1)
              case "^" => Cart(">", c1)
              case "<" => Cart("^", c1)
              case "v" => Cart("<", c1)
            }
          }
        }
      }

      override def toString: String = sign
    }

    def isMoving(s: String): Boolean = s match {
      case ">" => true
      case "<" => true
      case "^" => true
      case "v" => true
      case _ => false
    }

    def modify(track: Map[Int, Map[Int, Cart]], x: Int, y:Int, cart: Cart): Map[Int, Map[Int, Cart]] = {
      var xs = track.getOrElse(x, Map.empty[Int, Cart])
      xs.get(y) match {
        case None => {
          xs = xs + (y -> cart)
          track + (x -> xs)
        }
        case Some(s) => {
          if (isMoving(s.sign) && isMoving(cart.sign)) {
            throw new RuntimeException(s"Collision ($y, $x)")
          } else {
            if (!isMoving(s.sign)) {
              xs = xs + (y -> cart)
              track + (x -> xs)
            } else {
              track
            }
          }
        }
      }

    }

    Source.fromResource("thirteen").getLines.foreach(line => {
      var y = 0
      line.grouped(1).foreach(sign => {
        sign match {
          case ">" => track = modify(track, x, y, Cart(sign))
          case "<" => track = modify(track, x, y, Cart(sign))
          case "^" => track = modify(track, x, y, Cart(sign))
          case "v" => track = modify(track, x, y, Cart(sign))
          case "\\" => track = modify(track, x, y, Cart(sign))
          case "/" => track = modify(track, x, y, Cart(sign))
          case "+" => track = modify(track, x, y, Cart(sign))
          case _ =>

        }
        y = y  + 1
      })
      x = x + 1
    })

    val plainTrack = track.map(x => x._1 -> x._2.filter(y => !isMoving(y._2.sign)))

    def print(track: Map[Int, Map[Int, Cart]]): Unit = {
      val maxX = track.keys.max
      val maxY = track.flatMap(x => x._2.keys).max

      for (x <- 0 to maxX) {
          track.get(x) match {
            case None => println((0 to maxY).map(_ => " ").mkString)
            case Some(_) => {
              var line = ""
              for (y <- 0 to maxY) {
                line = line + track(x).getOrElse(y, " ")
              }
              println(line)
            }
          }
        }

      println()
      println()
      println()
    }

    def tick(track: Map[Int, Map[Int, Cart]]): Map[Int, Map[Int, Cart]] = {
      var newTrack = Map.empty[Int, Map[Int, Cart]]
      val maxX = track.keys.max
      val maxY = track.flatMap(x => x._2.keys).max

      for (x <- 0 to maxX) {
        track.get(x) match {
          case None =>
          case Some(_) => {
            for (y <- 0 to maxY) {
              track(x).get(y) match {
                case None =>
                case Some(s) => {
                  s.sign match {
                    case ">" => {
                      val n = getFromPlain(x, y)
                      if (n.isDefined) {
                        newTrack = modify(newTrack, x, y, n.get)
                      }
                      newTrack = modify(newTrack, x, y + 1, turn(s, getFromPlain(x, y +1)))
                    }
                    case "<" => {
                      val n = getFromPlain(x, y)
                      if (n.isDefined) {
                        newTrack = modify(newTrack, x, y, n.get)
                      }
                      newTrack = modify(newTrack, x, y - 1, turn(s, getFromPlain(x, y -1)))
                    }
                    case "^" => {
                      val n = getFromPlain(x, y)
                      if (n.isDefined) {
                        newTrack = modify(newTrack, x, y, n.get)
                      }
                      newTrack = modify(newTrack, x - 1, y, turn(s, getFromPlain(x - 1, y)))
                    }
                    case "v" => {
                      val n = getFromPlain(x, y)
                      if (n.isDefined) {
                        newTrack = modify(newTrack, x, y, n.get)
                      }
                      newTrack = modify(newTrack, x + 1, y, turn(s, getFromPlain(x + 1, y)))
                    }
                    case _ => newTrack = modify(newTrack, x, y, s)
                  }
                }
              }
            }
          }
        }
      }

      newTrack
    }

    def turn(sign: Cart, newPosSign: Option[Cart]): Cart = {
      newPosSign match {
        case None => sign
        case Some(x) => {
          x.sign match {
            case "\\" => {
              sign.sign match {
                case ">" => Cart("v", sign.c)
                case "v" => Cart(">", sign.c)
                case "<" => Cart("^", sign.c)
                case "^" => Cart("<", sign.c)
              }
            }
            case "/" => {
              sign.sign match {
                case ">" => Cart("^", sign.c)
                case "v" => Cart("<", sign.c)
                case "<" => Cart("v", sign.c)
                case "^" => Cart(">", sign.c)
              }
            }
            case "+" => sign.turn
          }
        }
      }
    }

    def getFromPlain(x: Int, y: Int): Option[Cart] = {
      plainTrack.get(x) match {
        case None => None
        case Some(xs) => xs.get(y) match {
          case None => None
          case Some(s) => Some(s)
        }
      }
    }

    for (i <- 1 to 1000) {
      track = tick(track)
    }
  }

  def main(args: Array[String]): Unit = {
    parse
  }
}
