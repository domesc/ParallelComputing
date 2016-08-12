package kmeans

import scala.collection.{GenMap, GenSeq}

object KM extends KMeans
import KM._

/**
  * Created by domesc on 31/07/16.
  */
object TestUtils {
  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  def equalPointSeq(expected: GenSeq[Point], actual: GenSeq[Point]) = {
    assert(expected == actual, "KMeans should return " + expected +
      " but it returned " + actual)
  }

}
