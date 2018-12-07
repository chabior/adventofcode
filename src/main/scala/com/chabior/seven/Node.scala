package com.chabior.seven

case class Node(label: Char) {
  var children = List.empty[Node]

  def addChildren(node: Node) = {
    children = children :+ node
    children = children.sortBy(_.label)
  }

  def isChild(node: Node) :Boolean = {
    children.find(_.label == node.label) match {
      case Some(_) => true
      case None => false
    }
  }

  def getNext: Node = {
    val next = children.head
    children = children.tail
    next
  }
}
