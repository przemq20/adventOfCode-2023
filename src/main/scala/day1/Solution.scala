package day1

import scala.annotation.tailrec
import scala.io.Source

object Solution extends App {

  @tailrec
  private def searchForFirst(str: String): Int = {
    str match {
      case _ if str(0).isDigit => str(0).asDigit
      case _                   => searchForFirst(str.drop(1))
    }
  }

  @tailrec
  private def searchForFirst2(str: String): Int = {
    str match {
      case _ if str(0).isDigit                          => str(0).asDigit
      case _ if str.slice(0, "one".length) == "one"     => 1
      case _ if str.slice(0, "two".length) == "two"     => 2
      case _ if str.slice(0, "three".length) == "three" => 3
      case _ if str.slice(0, "four".length) == "four"   => 4
      case _ if str.slice(0, "five".length) == "five"   => 5
      case _ if str.slice(0, "six".length) == "six"     => 6
      case _ if str.slice(0, "seven".length) == "seven" => 7
      case _ if str.slice(0, "eight".length) == "eight" => 8
      case _ if str.slice(0, "nine".length) == "nine"   => 9
      case _                                            => searchForFirst2(str.drop(1))
    }
  }

  @tailrec
  private def searchForLast(str: String): Int = {
    str match {
      case _ if str(str.length - 1).isDigit => str(str.length - 1).asDigit
      case _                                => searchForLast(str.dropRight(1))
    }
  }

  @tailrec
  private def searchForLast2(str: String): Int = {
    str match {
      case _ if str(str.length - 1).isDigit                                   => str(str.length - 1).asDigit
      case _ if str.slice(str.length - "one".length, str.length) == "one"     => 1
      case _ if str.slice(str.length - "two".length, str.length) == "two"     => 2
      case _ if str.slice(str.length - "three".length, str.length) == "three" => 3
      case _ if str.slice(str.length - "four".length, str.length) == "four"   => 4
      case _ if str.slice(str.length - "five".length, str.length) == "five"   => 5
      case _ if str.slice(str.length - "six".length, str.length) == "six"     => 6
      case _ if str.slice(str.length - "seven".length, str.length) == "seven" => 7
      case _ if str.slice(str.length - "eight".length, str.length) == "eight" => 8
      case _ if str.slice(str.length - "nine".length, str.length) == "nine"   => 9

      case _ => searchForLast2(str.dropRight(1))
    }
  }

  @tailrec
  private def getCalculation(leftLines: List[String], acc: Int = 0): Int = {
    leftLines match {
      case a if a.isEmpty => acc
      case _              =>
        val first = searchForFirst(leftLines.head)
        val last  = searchForLast(leftLines.head)
        getCalculation(leftLines.drop(1), acc + first * 10 + last)
    }
  }

  @tailrec
  private def getCalculation2(leftLines: List[String], acc: Int = 0): Int = {
    leftLines match {
      case a if a.isEmpty => acc
      case _              =>
        val first = searchForFirst2(leftLines.head)
        val last  = searchForLast2(leftLines.head)
        getCalculation2(leftLines.drop(1), acc + first * 10 + last)
    }
  }

  private def getSolution(fn: (List[String], Int) => Int, lines: List[String]): Unit = {
    println(fn(lines, 0))
  }
  private def read(day: Int, input: String = "input"): List[String] ={
    val bufferedSource = Source.fromFile(s"src/main/resources/day$day/$input.txt")
    val lines = bufferedSource.getLines().toList
    bufferedSource.close
    lines
  }


  getSolution(getCalculation, read(1, "testInput"))
  getSolution(getCalculation2, read(1, "testInput2"))
  getSolution(getCalculation, read(1))
  getSolution(getCalculation2, read(1))
}
