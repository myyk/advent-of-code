import com.github.myyk._

val input = readInput(2022,7)

val testInput = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""".split("\n").map(_.trim).drop(1).toSeq

case class File(name: String, size: Int) {
    override def toString: String = {
        s"$name (file, size=$size)"
    }

}

// When creating this, don't set dirs directly instead use withDir
class Directory(val name: String, var files: Set[File] = Set.empty, var dirs: Set[Directory] = Set.empty, val parent: Option[Directory]=None) {
    def totalSize: Int = {
        files.map(_.size).sum + dirs.map(_.totalSize).sum
    }

    def addFile(file: File): Unit = {
        files=files + file
    }

    def addDir(dir: Directory): Unit = {
        dirs.find(_.name == dir.name) match {
            case Some(_) => // do nothing
            case None => dirs=dirs + dir.withParent(this)
        }
    }

    def withParent(dir: Directory): Directory = {
        Directory(name, files, dirs, Some(dir))
    }

    def allFiles: List[File] = {
        files.toList ++ subdirFiles
    }

    def subdirFiles: List[File] = {
        dirs.map(_.files.toList).toList.flatten ++ dirs.map(_.subdirFiles).flatten
    }

    def flatten: List[Directory] = {
        var list = List.newBuilder[Directory]
        list.addOne(this)
        for (dir <- dirs) {
            list.addAll(dir.flatten)
        }
        list.result()
    }

    override def toString: String = {
        toStringHelper(new StringBuilder()).toString()
    }

    def toStringHelper(sb: StringBuilder, indent: Int = 0): StringBuilder = {
        sb.append("\n")
        sb.append("  " * indent)
        sb.append(s"- ${name} (dir)")
        for {dir <- dirs} {
            dir.toStringHelper(sb, indent+1)
        }
        for {file <- files} {
            sb.append("\n")
            sb.append("  " * (indent+1))
            sb.append("- ")
            sb.append(file.toString)
        }
        sb
    }
}

var testDir = Directory("\\")
testDir.addFile(File("A", 100))
testDir.addDir(Directory("foo", Set(File("B", 200))))
testDir.allFiles
testDir.flatten.map(_.name)
testDir.flatten.map(_.totalSize)

val cdCommandRegex = "\\$ cd (.*)".r
val dirOutputRegex = "dir (\\w+)".r
val fileOutputRegex = "(\\d+) (.*)".r

// test the regex
input.head match {
    case "$ cd /" => "root"
    case cdCommandRegex(dirName) => "not root"
    case _ => ???
}

input(2) match {
    case dirOutputRegex(dirName) => dirName
    case _ => ???
}

input(14) match {
    case fileOutputRegex(fileSize, fileName) => fileSize +  fileName
    case _ => ???
}

def readFileSystemFromConsoleOut(input: Seq[String]): Directory = {
    var currentDir = Directory("//")

    for {
        next <- input
    } {
        next match {
            case "$ cd /" =>
                while (currentDir.parent.nonEmpty) {
                    currentDir = currentDir.parent.get
                }
            case cdCommandRegex("..") => currentDir = currentDir.parent.get
            case cdCommandRegex(dirName) => 
                currentDir.addDir(Directory(dirName))
                currentDir = currentDir.dirs.find(_.name == dirName).get
            case "$ ls" =>
                // don't need to do anythign
            case dirOutputRegex(dirName) =>
                currentDir.addDir(Directory(dirName))
            case fileOutputRegex(fileSize, fileName) =>
                currentDir.addFile(File(fileName, fileSize.toInt))
            case anythingElse => throw new Exception(s"this was unhandled input: $anythingElse")
        } 
    }

    // set currentDir to root and then return it
    while (currentDir.parent.nonEmpty) {
        currentDir = currentDir.parent.get
    }

    return currentDir
}

val testRoot = readFileSystemFromConsoleOut(testInput)

val root = readFileSystemFromConsoleOut(input)

// find all total size of all files with at most 100000 size
val largestSize = 100000;

def dirsWithTotalSizeLessThan(root: Directory, largestSize: Int): List[Directory] = {
    root.flatten.filter(_.totalSize <= largestSize)
}

val testAnswer1 = dirsWithTotalSizeLessThan(testRoot, largestSize).map(_.totalSize).sum
assert(testAnswer1 == 95437)

val answer1 = dirsWithTotalSizeLessThan(root, largestSize).map(_.totalSize).sum
// 1348005

val totalDiskSpaceAvailable = 70000000
val toRunUpdate = 30000000

def findSmallestDirToDelete(root: Directory): Directory = {
    val spaceNeededToFree = toRunUpdate - (totalDiskSpaceAvailable - root.totalSize)
    assert(spaceNeededToFree > 0)
    root.flatten.sortBy(_.totalSize).find(_.totalSize > spaceNeededToFree).get
}

val testAnswer2 = findSmallestDirToDelete(testRoot).totalSize
assert(testAnswer2 == 24933642)

val answer2 = findSmallestDirToDelete(root).totalSize
// 12785886