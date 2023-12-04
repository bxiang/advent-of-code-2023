package com.aoc

import scala.io.Source
import scala.math.pow

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
  val x = s.split(": ").last.split("""\| """)
  val ws = x.head.split(" ").toList.map(_.trim).filter(_.nonEmpty)
  val ns = x.last.split(" ").toList.map(_.trim).filter(_.nonEmpty)
  pow(2, ws.intersect(ns).length - 1).toInt

def parseLinePart2(s: String): Int =
  val x = s.split(": ").last.split("""\| """)
  val ws = x.head.split(" ").toList.map(_.trim).filter(_.nonEmpty)
  val ns = x.last.split(" ").toList.map(_.trim).filter(_.nonEmpty)
  ws.intersect(ns).length

