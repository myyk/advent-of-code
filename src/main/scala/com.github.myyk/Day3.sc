import scala.io.Source

//TODO: figure out how to reuse the input reading
val projectBase = "/Users/myyk.seok/workspace/advent-of-code"
val sampleDir = projectBase + "/samples"

def readInput(day: Int):Seq[String] = {
  Source.fromFile(s"$sampleDir/day$day.txt").getLines.toSeq
}

val inputs = readInput(3)

val claim = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r
case class Claim(id: Int, offsetX: Int, offsetY: Int, sizeX: Int, sizeY: Int)
val claims = for {
  input <- inputs
} yield {
  val claim(id, offsetX, offsetY, sizeX, sizeY) = input
  Claim(id.toInt, offsetX.toInt, offsetY.toInt, sizeX.toInt, sizeY.toInt)
}

val fabricUsages = Array.fill(1000)(Array.fill(1000)(0))
for {
  claim <- claims
  x <- claim.offsetX until claim.offsetX+claim.sizeX
  y <- claim.offsetY until claim.offsetY+claim.sizeY
} {
  fabricUsages(x)(y) += 1
}

// Answer 1
val overusedFabric = fabricUsages.flatten.count(_>1)

def doesNotOverlap(claim: Claim): Boolean = {
  val usages = for {
    x <- claim.offsetX until claim.offsetX+claim.sizeX
    y <- claim.offsetY until claim.offsetY+claim.sizeY
  } yield {
    fabricUsages(x)(y)
  }
  usages.forall(_ <= 1)
}

val nonOverlappingClaims = claims.filter(doesNotOverlap)
if (nonOverlappingClaims.size != 1) { throw new Exception}

// Answer 2
val nonOverlappingClaim = nonOverlappingClaims.last