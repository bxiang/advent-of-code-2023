package com.aoc

import scala.io.Source

@main
def day3() =
  val (numbers, symbols) = Source
    .fromResource("input3.txt")
    .getLines
    .zipWithIndex
    .map(r => parseLinePart1(r._1, r._2))
    .reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
  val part1 = findPartNumbers(numbers, symbols)
  println(s"Part1: $part1")
  val (numbers2, stars) = Source
    .fromResource("input3.txt")
    .getLines
    .zipWithIndex
    .map(r => parseLinePart2(r._1, r._2))
    .reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
  val part2 = findGearRatio(numbers2, stars)
  println(s"Part2: $part2")

def findPartNumbers(
    numbers: List[Number],
    symbols: List[Symbol]
): Int =
  numbers
    .filter(n =>
      val adjacentNumbers = (n.x0 - 1 to n.x1 + 1).toList.flatMap(x =>
        List(Symbol(x, n.y - 1), Symbol(x, n.y + 1))
      ) ++ List(Symbol(n.x0 - 1, n.y), Symbol(n.x1 + 1, n.y))
      symbols.intersect(adjacentNumbers).nonEmpty
    )
    .map(_.value.toInt)
    .reduce(_ + _)

def findGearRatio(
    numbers: List[Number],
    stars: List[Symbol]
): Int =
  stars
    .map { s =>
      val adjNumbers = numbers.filter(n =>
        ((n.x0 <= s.x + 1) && (n.x0 >= s.x - 1) || (n.x1 <= s.x + 1) && (n.x1 >= s.x - 1)) && (n.y == s.y - 1 || n.y == s.y || n.y == s.y + 1)
      )
      if (adjNumbers.size == 2)
        adjNumbers.head.value.toInt * adjNumbers.last.value.toInt
      else 0
    }
    .reduce(_ + _)

def parseLinePart2(line: String, index: Int): (List[Number], List[Symbol]) =
  val numbers = parseLineNumbers(line).map(_.copy(y = index))
  val symbols = parseLineStars(line).map(_.copy(y = index))
  (numbers, symbols)

def parseLinePart1(line: String, index: Int): (List[Number], List[Symbol]) =
  val numbers = parseLineNumbers(line).map(_.copy(y = index))
  val symbols = parseLineSymbols(line).map(_.copy(y = index))
  (numbers, symbols)

def parseLineNumbers(line: String): List[Number] =
  line.zipWithIndex.foldLeft(List.empty[Number]) { (a, c) =>
    if (c._1.isDigit) {
      if (a.lastOption.exists(_.x1 == c._2 - 1))
        a.dropRight(1) :+ Number(a.last.value + c._1, a.last.x0, c._2, 0)
      else
        a :+ Number("" + c._1, c._2, c._2, 0)
    } else a
  }

def parseLineStars(line: String): List[Symbol] =
  line.zipWithIndex.foldLeft(List.empty[Symbol]) { (a, c) =>
    if (c._1 == '*') {
      a :+ Symbol(c._2, 0)
    } else a
  }

def parseLineSymbols(line: String): List[Symbol] =
  line.zipWithIndex.foldLeft(List.empty[Symbol]) { (a, c) =>
    if (!c._1.isDigit && c._1 != '.') {
      a :+ Symbol(c._2, 0)
    } else a
  }

final case class Symbol(x: Int, y: Int)
final case class Number(value: String, x0: Int, x1: Int, y: Int)
