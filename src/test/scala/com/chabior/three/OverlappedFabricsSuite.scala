package com.chabior.three

import org.scalatest.FlatSpec

class OverlappedFabricsSuite extends FlatSpec {
  "overlapped fabrics" should "count duplicated claims" in {
    assert(OverlappedFabrics.count(List(Fabric(1, 1, 3, 4, 4), Fabric(2, 3, 1, 4, 4), Fabric(3, 5, 5, 2, 2))) == 4)
    assert(OverlappedFabrics.count(InputParser.parse) == 116491)
  }

  it should "find not overlapping" in {
    assert(OverlappedFabrics.findNot(List(Fabric(1, 1, 3, 4, 4), Fabric(2, 3, 1, 4, 4), Fabric(3, 5, 5, 2, 2))) == 3)
    assert(OverlappedFabrics.findNot(InputParser.parse) == 707)
  }
}
