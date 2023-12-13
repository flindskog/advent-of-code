package aoc.syntax

import aoc.utils.Text

trait StringSyntax {
  extension (s: String) {
    def levenshteinDistance(o: String): Int = Text.levenshteinDistance(s, o)
  }
}
