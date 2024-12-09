package aoc.y2022

object Day07 extends Aoc2022("input_07.txt"):

  enum FileTree:
    case File(name: String, size: Int)
    case Directory(name: String, children: List[FileTree])
