import com.github.myyk._

val input = readInput(2022,3)

val rucksacks = {
    for {
        line <- input
        compartment <- line.toCharArray.toSeq.grouped(line.size/2)
    } yield {
        compartment
    }
}.grouped(2).toSeq

val errors = for {
    Seq(compartment1, compartment2) <- rucksacks
} yield {
        val intersection = compartment1.toSet intersect compartment2.toSet
        if (intersection.size != 1) {
            throw new Exception("unexpected")
        } 
        intersection.toSeq.head
}

def itemToPriority(c: Char): Int = {
    // Lowercase item types a through z have priorities 1 through 26.
    // Uppercase item types A through Z have priorities 27 through 52.
    if (c >= 'a' && c <= 'z') {
        c - 'a' + 1
    } else {
        c - 'A' + 27
    }
}

itemToPriority('a')
itemToPriority('z')
itemToPriority('A')
itemToPriority('Z')

val answer1 = errors.toSeq.map(itemToPriority).sum
// 8123

val noncompartmentalRucksacks = for {
    Seq(compartment1, compartment2) <- rucksacks
} yield {
    compartment1.toSet | compartment2.toSet
}

val badges = for {
    Seq(a, b, c) <- noncompartmentalRucksacks.grouped(3)
} yield {
   val intersection = a & b & c
    if (intersection.size != 1) {
        throw new Exception("unexpected")
    } 
    intersection.toSeq.head
}

val answer2 = badges.toSeq.map(itemToPriority).sum
// 2620