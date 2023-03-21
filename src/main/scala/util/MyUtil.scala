package util

import scala.annotation.tailrec

object MyUtil {
  type Sir = List[Char]

  // for List[Char]
  def opSplit(splitChar: Char)(c: Char, acc: List[Sir]): List[Sir] = {
    acc match {
      case Nil => if (c == splitChar) Nil else List(List(c))
      case x :: xs =>
        if (c == splitChar) Nil :: acc
        else (c :: x) :: xs
    }
  }
}
