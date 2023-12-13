package aoc.syntax

trait AllSyntax extends OptionSyntax with SeqSyntax

object all    extends AllSyntax
object option extends OptionSyntax
object seq    extends SeqSyntax
