package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.collection.mutable

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @annotation.tailrec
    def balanceRec(chars: Array[Char], from: Int, until: Int, acc: Int): Int = {
      if (from > until || acc < 0) acc
      else {
        if (chars(from) == '(') balanceRec(chars, from + 1, until, acc + 1)
        else if (chars(from) == ')') balanceRec(chars, from + 1, until, acc - 1)
        else balanceRec(chars, from + 1, until, acc)
      }
    }
    balanceRec(chars, 0, chars.length - 1, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, acc: Int): Int = {
      if (idx >= until || acc < 0) acc
      else {
        if (chars(idx) == '(') traverse(idx + 1, until, acc + 1)
        else if (chars(idx) == ')') traverse(idx + 1, until, acc - 1)
        else traverse(idx + 1, until, acc)
      }
    }

    def reduce(from: Int, until: Int): Int = {
      if ((until - from) < 0) 0
      else if (until - from <= threshold) {
        traverse(from, until, 0)
      } else {
        val mid = (until + from) / 2
        val (acc1, acc2) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        if(acc1 < 0) acc1 else acc1 + acc2
      }
    }
    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
