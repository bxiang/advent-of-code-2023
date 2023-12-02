package com.aoc.day2

import scala.io.Source

@main
def main() =
  val part1 = Source
    .fromResource("input2.txt")
    .getLines
    .map(parseLine)
    .filter(isPossible)
    .foldLeft(0)((c, g) => c + g.id)
  println(s"Part1: $part1")
  val part2 = Source
    .fromResource("input2.txt")
    .getLines
    .map(parseLine)
    .map(toPower)
    .reduce(_ + _)
  println(s"Part2: $part2")

def parseLine(s: String): Game =
  val Array(gameInfo, cubesInfo) = s.split(": ")
  val id = gameInfo.split(" ").last.toInt
  val cubSets = cubesInfo
    .split("; ")
    .toList
    .map(_.split(", ").toList.map { r =>
      val Array(num, color) = r.split(" ")
      Cubes(num.toInt, color)
    })
  Game(id, cubSets)

def isPossible(game: Game): Boolean =
  game.cubeSets.forall(
    _.forall(c => bag.find(_.color == c.color).exists(_.count >= c.count))
  )

def toPower(game: Game): Int =
  game.cubeSets.flatten
    .foldLeft(Map.empty[String, Int])((c, g) =>
      val count = c.get(g.color).getOrElse(g.count)
      if (g.count >= count) c + (g.color -> g.count) else c
    )
    .foldLeft(1)(_ * _._2)

val bag = List(Cubes(12, "red"), Cubes(13, "green"), Cubes(14, "blue"))

final case class Cubes(count: Int, color: String)
final case class Game(id: Int, cubeSets: List[List[Cubes]])
