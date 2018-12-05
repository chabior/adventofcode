package com.chabior.five

import org.scalatest.FlatSpec

class ReactorSuite extends FlatSpec {
  it should "find most sleep minute" in {
    assert(Reactor.analyze("dabAcCaCBAcCcaDA") == 10)
//    assert(Reactor.lengthOfBest("dabAcCaCBAcCcaDA") == 4)
  }
}
