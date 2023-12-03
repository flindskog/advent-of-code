import util.Input

enum Color {
  case red, green, blue
}

trait Day02 {
  val data = Input.read("input_02.txt")

  val regex = "^Game (\\d+): (.*)$".r

  val games = data
    .flatMap(row => regex.findFirstMatchIn(row).map(m => m.group(1) -> m.group(2)))
    .map { (game, row) =>
      val sets = row
        .split("; ")
        .map { set =>
          set
            .split(", ")
            .map { draw =>
              val Array(count, color) = draw.split(" ")
              Color.valueOf(color) -> count.toInt
            }
            .toMap
        }
        .toList

      game.toInt -> sets
    }
}

object Day02_1 extends App with Day02 {
  val dices = Map(
    Color.red   -> 12,
    Color.green -> 13,
    Color.blue  -> 14
  )

  val validGames = games.filter { (_, sets) =>
    sets.forall { set =>
      set.forall { (color, count) =>
        count <= dices(color)
      }
    }
  }

  println(validGames.map((game, _) => game).sum)
}

object Day02_2 extends App with Day02 {
  val minDices = games.map { (game, sets) =>
    val dices = sets.flatten
      .groupBy((color, _) => color)
      .view
      .mapValues(_.map((_, count) => count).max)
      .toMap

    game -> dices
  }

  val powerSum = minDices.map { (_, dices) =>
    dices.values.product
  }.sum

  println(powerSum)
}
