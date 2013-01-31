package org.example

import annotation.tailrec
import com.google.caliper.Param

class Benchmark extends SimpleScalaBenchmark {
  import scala.collection.mutable.ArrayStack
 
  @Param(Array("0", "1", "2", "3", "5", "10", "100", "500", "1000", "2000"))
  val length: Int = 0

  @Param(Array("3", "5", "10", "20", "100"))
  val threshold: Int = 0

 
  var list: List[Int] = _
 
  override def setUp() {
    list = (0 to length).toList
  }
 
  val addValues = (left: Int, right: Int) => left + right
 
  def foldRightAlternative[A, B](list: List[A])(z: B)(f: (A, B) => B): B =
    if (list.lengthCompare(threshold) < 0)
      list.foldRight(z)(f)
    else
      list.reverse.foldLeft(z)((right, left) => f(left, right))
 
  def timecurrentFoldRight(reps: Int) = repeat(reps)(list.foldRight(0)(addValues))
 
  def timecurrentReverseFoldLeft(reps: Int) = repeat(reps)(list.reverse.foldLeft(0)((right, left) => addValues(left, right)))
  
  def timefoldRightAlternative(reps: Int) = repeat(reps)(foldRightAlternative(list)(0)(addValues))
}

