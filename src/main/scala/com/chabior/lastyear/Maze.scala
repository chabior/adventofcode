package com.chabior.lastyear

import scala.io.Source

object Maze {
  def main(args: Array[String]): Unit = {
    val list = new LinkedList
    Source.fromResource("lastyear/maze").getLines.foreach(x => {
      list.append(x.toInt)
    })

    println(list.jump)

    println(list.head)
  }
}

class LinkedList {
  var head : Option[Node] = None

  def append(d: Int) = {
    head match {
      case None => head = Some(Node(d))
      case Some(x) => {
        val node = Node(d)
        x.next = Some(node)
        node.prev = Some(x)
        head = Some(node)
      }
    }
  }

  def moveToStart: Unit = {
    head match {
      case None => throw new RuntimeException
      case Some(x) => {
        var temp = x
        while (temp.prev.isDefined) {
          head = temp.prev
          temp = head.get
        }
      }
    }
  }

  def jump: Int = {
    var d = 0
    head match {
      case None => throw new RuntimeException
      case Some(x) => {
        while(true) {
          var temp = x
          val steps = temp.d
          temp.d = temp.d + 1
          steps match {
            case x if x > 0 => {
              for (x <- 1 to steps) {
                temp.next match {
                  case None => return d
                  case Some(x) => {
                    temp = x
                  }
                }
              }
            }
            case x if x < 0 => {
              for (x <- 1 to steps) {
                temp.prev match {
                  case None => return d
                  case Some(x) => {
                    temp = x
                  }
                }
              }
            }
            case 0 =>
          }
          d = d + 1
        }
      }
    }
    d
  }

  case class Node(var d: Int) {
    var next: Option[Node] = None
    var prev: Option[Node] = None
  }
}
