import scala.io.Source

object InputParser {
    def parse: List[Fabric] = {
      Source.fromResource("fabrics").getLines.map(x => {
        val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
        val pattern(clientId, left, top, width, height) = x
        Fabric(clientId.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
      }).toList
    }
}
