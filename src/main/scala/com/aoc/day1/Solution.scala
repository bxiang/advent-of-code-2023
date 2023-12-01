package com.aoc.day1

import scala.io.Source

@main
def main() =
  val part1 = Source
    .fromResource("day1/input.txt")
    .getLines
    .map(findNumbersPart1)
    .reduce(_ + _)
  println(s"Part1: $part1")
  val part2 = Source
    .fromResource("day1/input.txt")
    .getLines
    .map(findNumbersPart2)
    .reduce(_ + _)
  println(s"Part2: $part2")

def findNumbersPart1(s: String): Int =
  val first = s.find(_.isDigit).getOrElse('0')
  val last = s.findLast(_.isDigit).getOrElse('0')
  s"$first$last".toInt

def findNumbersPart2(s: String): Int =
  val first = digitsMap.get(regexFirst.findFirstIn(s).get).getOrElse(0)
  val last = digitsMap.get(regexLast.findFirstIn(s.reverse).get.reverse).getOrElse(0)
  first * 10 + last

val digitsMap = Map(
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9
) ++ (1 to 9).map(i => i.toString -> i)

val regexFirst = digitsMap.keys.mkString("|").r
val regexLast = digitsMap.keys.map(_.reverse).mkString("|").r
