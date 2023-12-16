package aoc.syntax

trait AllSyntax extends OptionSyntax with SeqSyntax with StringSyntax with GridSyntax

object all    extends AllSyntax
object option extends OptionSyntax
object seq    extends SeqSyntax
object string extends StringSyntax
object grid   extends GridSyntax
