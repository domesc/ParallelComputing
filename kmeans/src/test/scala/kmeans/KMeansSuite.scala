package kmeans

import kmeans.KM._
import kmeans.TestUtils._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point,GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("Converged function should fail in case we pass lists with different sizes") {
    val newMeans: GenSeq[Point] = GenSeq(new Point(0, 0, 1), new Point (0,0, -1), new Point(0,1,0), new Point(0,10,0))
    val oldMeans: GenSeq[Point] = GenSeq(new Point(0, -1, 0), new Point(0, 2, 0))
    val eta = 12.25

    intercept[IllegalArgumentException]{
      converged(eta)(oldMeans, newMeans)
    }
  }

  test("Converged function return true because lists are the same") {
    val newMeans: GenSeq[Point] = GenSeq(new Point(0, 0, 1), new Point (0,0, -1), new Point(0,1,0), new Point(0,10,0))
    val oldMeans: GenSeq[Point] = GenSeq(new Point(0, 0, 1), new Point (0,0, -1), new Point(0,1,0), new Point(0,10,0))
    val eta = 0.001

    assert(converged(eta)(oldMeans, newMeans))
  }

  test("kMeans should work for points == GenSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0)) " +
    "and 'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12.25") {
    val points: GenSeq[Point] = GenSeq(new Point(0, 0, 1), new Point (0,0, -1), new Point(0,1,0), new Point(0,10,0))
    val oldMeans: GenSeq[Point] = GenSeq(new Point(0, -1, 0), new Point(0, 2, 0))
    val eta = 12.25
    val expected: GenSeq[Point] = GenSeq(new Point(0.0, 0.0, 0.0), new Point(0.0, 5.5, 0.0))

    val actual: GenSeq[Point] = kMeans(points, oldMeans, eta)

    equalPointSeq(expected, actual)
  }

  test("kMeans should work for parallel points == GenSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0)) " +
    "and 'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12.25") {
    val points: GenSeq[Point] = GenSeq(new Point(0, 0, 1), new Point (0,0, -1), new Point(0,1,0), new Point(0,10,0))
    val oldMeans: GenSeq[Point] = GenSeq(new Point(0, -1, 0), new Point(0, 2, 0))
    val eta = 12.25
    val expected: GenSeq[Point] = GenSeq(new Point(0.0, 0.0, 0.0), new Point(0.0, 5.5, 0.0))

    val actual: GenSeq[Point] = kMeans(points.par, oldMeans.par, eta)

    equalPointSeq(expected, actual)
  }
}


  
