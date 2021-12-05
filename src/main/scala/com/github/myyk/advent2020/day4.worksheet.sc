import com.github.myyk.advent2020._

val input = com.github.myyk.readInput(2020,4)

var groupedInput = Vector.empty[String]

{
  var nextPassport = Vector.empty[String]
  for {
    nextLine <- input
  } yield {
    if (nextLine.isEmpty) {
      groupedInput = groupedInput :+ nextPassport.mkString(" ")
      nextPassport = Vector.empty[String]
    } else {
      nextPassport = nextPassport :+ nextLine
    }
  }
  if (nextPassport.nonEmpty) {
    groupedInput = groupedInput :+ nextPassport.mkString(" ")
  }
}

val passports = for {
  passport <- groupedInput
} yield {
  val kvPairs = for {
    pair <- passport.split(" ")
  } yield {
    val thePair =  pair.split(":")
    (thePair(0), thePair(1))
  }
  kvPairs.toMap
}

val requiredKeys = Seq(
  "byr", //(Birth Year)
  "iyr", //(Issue Year)
  "eyr", //(Expiration Year)
  "hgt", //(Height)
  "hcl", //(Hair Color)
  "ecl", //(Eye Color)
  "pid", //(Passport ID)
)
val optionalKeys = Seq(
  "cid", //(Country ID)
)

val passports0 = passports(0)
val passports1 = passports(1)
val passportsLast = passports.last

val (containValidFields, invalid) = passports.partition{ passport =>
  requiredKeys.forall(passport.contains)
}

// Answer 1
val answer1 = containValidFields.size
//250

def betweenInts(minInclusive: Int, maxInclusive:Int)(str:String):Boolean = {
  val intVal = str.toInt
  minInclusive <= intVal && intVal <= maxInclusive
}

def validHeight(str:String):Boolean = {
  val cm = raw"(\d+)cm".r
  val in = raw"(\d+)in".r

  str match {
    case cm(centimeters) =>
      betweenInts(150, 193)(centimeters)
    case in(inches) =>
      betweenInts(59, 76)(inches)
    case _ => false
  }
}

def validHairColor(str:String):Boolean = {
  val r = raw"^#([0-9a-f]{6})$$".r

  r.matches(str)
}

val validEyeColors = Set(
  "amb" ,"blu" ,"brn" ,"gry", "grn", "hzl", "oth"
)

def validPassportID(str:String):Boolean = {
  val r = raw"^(\d{9})$$".r

  r.matches(str)
}


val validations = Map(
  "byr" -> betweenInts(1920, 2002) _,
  "iyr" -> betweenInts(2010, 2020) _,
  "eyr" -> betweenInts(2020, 2030) _,
  "hgt" -> validHeight _,
  "hcl" -> validHairColor _,
  "ecl" -> validEyeColors.contains _,
  "pid" -> validPassportID _,
)

val (valid, invalid2) = containValidFields.partition{ passport =>
  validations.forall{case (key, validation) => validation(passport(key))}
}

// Answer 2
val answer2 = valid.size
//158