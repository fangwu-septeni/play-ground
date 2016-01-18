object ScalaOptimizationTest {
  import TestInvariant._
  import Benchmark._

  def main(args: Array[String]): Unit = {
    val testSize = args(0).toInt
    val s1 = (1 to testSize).filter(_ % 2 == 0).toSeq
    val s2 = (1 to testSize).filter(_ % 7 == 0).toSeq
    val result = (
      benchmark(forIntersect1(s1, s2)),
      benchmark(forIntersect2(s1, s2)),
      benchmark(intersect1(s1, s2)),
      benchmark(intersect2(s1, s2))
    )
    println(
      s"""
       | Benchmark result:
       |  1. Traditional looping: ${result._1.toString}
       |  2. For comprehension: ${result._2.toString}
       |  3. Naive mapping: ${result._3.toString}
       |  4. Mapping (with toSet moveout): ${result._4.toString}
        """
    )
  }
}

object TestInvariant {
  import scala.collection.mutable.ListBuffer
  def forIntersect1(s1: Seq[Int], s2: Seq[Int]): Seq[Int] = {
    val result: ListBuffer[Int] = ListBuffer()
    for (x <- 0 until s1.length) {
      for (y <- 0 until s2.length) {
        if (s1(x) == s2(y))
          result += s1(x)
      }
    }
    return result.toSeq
  }
  def forIntersect2(s1: Seq[Int], s2: Seq[Int]): Seq[Int] = {
    for {
      x <- s1
      y <- s2 if x == y
    } yield y
  }
  def intersect1(s1: Seq[Int], s2: Seq[Int]): Seq[Int] =
    s2.filter(elem => s1.toSet.contains(elem))
  def intersect2(s1: Seq[Int], s2: Seq[Int]): Seq[Int] = {
    val set = s1.toSet
    s2.filter(elem => set.contains(elem))
  }
}

object Benchmark {
  import java.lang.System

  def benchmark(block: => Unit): (Double, Double) = {
    (1 to 3).foreach(_ => block) // Let the JIT engine optimize the hotspot
    val results = (1 to 31).map{_ => time(block)}
    (mean(results), variance(results))
  }

  private def time(block: => Unit): Long = {
    val t0 = System.nanoTime()
    block
    val result = System.nanoTime() - t0
    System.gc()
    result
  }

  private def mean(seq: Seq[Long]): Double =
    seq.sum / seq.length

  private def variance(seq: Seq[Long]): Double = {
    val m = mean(seq)
    math.sqrt(seq.map(elem => scala.math.pow(elem - m, 2)).sum/seq.length)
  }
}
