package com.aoc

import scala.io.Source

@main
def day04() =
  val part1 = Source
    .fromResource("input4.txt")
    .getLines
    .map(parseLinePart1)
    .reduce(_ + _)
  println(s"Part1: $part1")
  val part2 = Source
    .fromResource("input4.txt")
    .getLines
    .map(parseLinePart2)
    .foldLeft(List.empty[(Int, Int)])((a, b) => {
      val copies = a.filter(_._1 > 0).foldLeft(0)(_ + _._2) + 1
      a.map(r => if (r._1 > 0) (r._1 - 1, r._2) else r) :+ (b, copies)
    }).foldLeft(0)(_ + _._2)
  println(s"Part2: $part2")

def parseLinePart1(s: String): Int =
  val x = s.split(": ").last
  val ws =
    x.takeWhile(_ != '|').split(" ").toList.map(_.trim).filter(_.nonEmpty)
  val ns = x
    .dropWhile(_ != '|')
    .drop(1)
    .split(" ")
    .toList
    .map(_.trim)
    .filter(_.nonEmpty)
  ws.intersect(ns).foldLeft(0)((a, b) => if (a == 0) 1 else a * 2)

def parseLinePart2(s: String): Int =
  val x = s.split(": ").last
  val ws =
    x.takeWhile(_ != '|').split(" ").toList.map(_.trim).filter(_.nonEmpty)
  val ns = x
    .dropWhile(_ != '|')
    .drop(1)
    .split(" ")
    .toList
    .map(_.trim)
    .filter(_.nonEmpty)
  ws.intersect(ns).length
