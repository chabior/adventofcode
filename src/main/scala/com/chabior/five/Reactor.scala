package com.chabior.five

object Reactor {
  def analyze(polymer: String): Int = {
    def f(p: String) :String = {
      findToRemove(p) match {
        case Some(i) => f(remove(remove(p, i), i))
        case None => p
      }
    }

    f(polymer).length
  }

  def lengthOfBest(polymer: String): Int = {
    println(s"All: ${polymer.toLowerCase.distinct}")
    var smallest: Int = 99999
    var i = 0
    for (letter <- polymer.toLowerCase.distinct) {
      println(i)
      i = i + 1
      val newPolymer = polymer.filterNot(x => x == letter || x == letter.toUpper)
      val newPolymerLength = analyze(newPolymer)
      if (newPolymerLength < smallest) {
        smallest = newPolymerLength
      }
    }
    smallest
  }

  private def remove(polymer: String, i: Int) : String = {
    polymer.toList.splitAt(i) match {
      case (Nil, _) if i < 0 => throw new NoSuchElementException
      case (pre, e :: post) => (pre ::: post).mkString
      case (pre, Nil) => throw new NoSuchElementException
    }
  }

  private def findToRemove(polymer: String): Option[Int] = {
    (0 until polymer.length - 1).find(i => {
      val curr = polymer.charAt(i)
      val next = polymer.charAt(i + 1)
      curr.toLower.equals(next.toLower) && ((curr.isLower && next.isUpper) || (curr.isUpper && next.isLower))
    })
  }

}
