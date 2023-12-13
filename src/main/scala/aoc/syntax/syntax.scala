package aoc.syntax

trait AllSyntax extends OptionSyntax with SeqSyntax with StringSyntax

object all    extends AllSyntax
object option extends OptionSyntax
object seq    extends SeqSyntax
object string extends StringSyntax
