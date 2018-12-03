object App {
  def main(args: Array[String]): Unit = {
    println(OverlappedFabrics.findNot(InputParser.parse))
  }
}
