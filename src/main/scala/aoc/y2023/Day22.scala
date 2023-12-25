package aoc.y2023

import aoc.data.{DirectedGraph, Point3d}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day22 extends Aoc2023("input_22.txt"):
  case class Brick(coord1: Point3d[Int], coord2: Point3d[Int]) {
    val lowestLayer: Int  = coord1.z.min(coord2.z)
    val highestLayer: Int = coord1.z.max(coord2.z)
    def transposeToLayer(layer: Int): Brick =
      val zDiff = layer - lowestLayer
      copy(
        coord1 = coord1.copy(z = coord1.z + zDiff),
        coord2 = coord2.copy(z = coord2.z + zDiff)
      )

    def intersectsInXyPlane(other: Brick): Boolean =
      val xIntersects = coord1.x <= other.coord2.x && coord2.x >= other.coord1.x
      val yIntersects = coord1.y <= other.coord2.y && coord2.y >= other.coord1.y
      xIntersects && yIntersects
  }

  val bricks = input.map { line =>
    val Array(coord1, coord2) = line.split("~")
    val coord1Array           = coord1.split(",").map(_.toInt)
    val coord2Array           = coord2.split(",").map(_.toInt)
    Brick(
      Point3d(coord1Array(0), coord1Array(1), coord1Array(2)),
      Point3d(coord2Array(0), coord2Array(1), coord2Array(2))
    )
  }.toList.sortBy(_.coord1.z).sortBy(_.coord2.z)

  val fallenBricks = bricks
    .foldLeft(List.empty[Brick]) { (acc, brick) =>
      val freeLayer =
        acc
          .filter(_.intersectsInXyPlane(brick))
          .map(_.highestLayer)
          .sorted
          .reverse
          .headOption
          .getOrElse(0) + 1

      brick.transposeToLayer(freeLayer) :: acc
    }

  val fallenByHighestLayer = fallenBricks.groupBy(_.highestLayer)

  val notSafeToRemove = fallenByHighestLayer.values.flatten.flatMap { brick =>
    fallenByHighestLayer
      .get(brick.lowestLayer - 1)
      .toList
      .map { bricks =>
        bricks.filter(_.intersectsInXyPlane(brick))
      }
      .filter(_.size == 1)
      .flatten
  }.toSet

  val safeToRemove = fallenByHighestLayer.values.flatten.toSet -- notSafeToRemove

  println(safeToRemove.size) // 375

  // part 2
  val fallenByLowestLayer = fallenBricks.groupBy(_.lowestLayer)

  val graph = fallenBricks.map { brick =>
    brick -> fallenByLowestLayer
      .get(brick.highestLayer + 1)
      .toList
      .flatMap { bricks =>
        bricks.filter(_.intersectsInXyPlane(brick))
      }
  }.foldLeft(DirectedGraph.empty[Brick]) { case (graph, (brick, bricksIAmSupporting)) =>
    bricksIAmSupporting.foldLeft(graph) { case (graph, brickIAmSupporting) =>
      graph.addEdge(brick, brickIAmSupporting)
    }
  }

  val inverse = graph.inverse

  def falling(brick: Brick): Set[Brick] = {
    @tailrec
    def loop(queue: Queue[Brick], removed: Set[Brick], falling: Set[Brick]): Set[Brick] =
      queue match {
        case Queue() => falling
        case Queue(brick, _*) =>
          val bricksIAmSupporting = graph.edgesByKey.getOrElse(brick, Set()).map(_.to)
          val nextLayerFalling = bricksIAmSupporting.filter { supportedBrick =>
            val supportedBy = inverse.edgesByKey.getOrElse(supportedBrick, Set()).map(_.to)
            supportedBy.subsetOf(removed)
          }
          val newQueue   = queue.tail.enqueueAll(nextLayerFalling)
          val newRemoved = removed ++ nextLayerFalling
          val newFalling = falling ++ nextLayerFalling
          loop(newQueue, newRemoved, newFalling)
      }

    loop(Queue(brick), Set(brick), Set())
  }

  val result = fallenBricks.map(brick => falling(brick).size).sum
  println(result) // 72352
