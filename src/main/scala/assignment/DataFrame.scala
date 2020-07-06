package assignment

import assignment.Table
import scala.io.Source
import scala.annotation.tailrec


class DataFrame (private val data: Table) {
  /**
   * Returns a set of statistics (min, max, average and standard deviation) about
   * the given feature. See exercise 2.9 (Standard deviation).
   * Note: built-in functions map and reduce allowed here.
   *
   * @param i Feature/attribute index for which the statistics are to be computed.
   * @return An object of type Stats containing the statistics (see Stats.scala).
   */
  def stats(i: Int): Stats = {
    /**
     * Returns minimum value of a list of elements in a given feature
     *
     * @param l list of elements in given feature i mentioned in the parent function.
     * @return double value min
     */
    def calculateMin(l: List[Double]): Double = {
      @tailrec
      def minAcc(l: List[Double], currMin: Double): Double = {
        l match {
          case Nil => currMin
          case head :: tail =>
            val nexMin = if (head < currMin) head else currMin
            minAcc(tail, nexMin)
        }
      }
      minAcc(l, l.head)
    }

    /**
     * Returns maximum value of a list of elements in a given feature
     *
     * @param l list of elements in given feature i mentioned in the parent function.
     * @return double value max
     */
    def calculateMax(l: List[Double]): Double = {
      @tailrec
      def maxAcc(l: List[Double], currMax: Double): Double = {
        l match {
          case Nil => currMax
          case head :: tail =>
            val nexMax = if (head > currMax) head else currMax
            maxAcc(tail, nexMax)
        }
      }
      maxAcc(l, l.head)
    }

    /**
     * Returns mean value of a list of elements in a given feature
     *
     * @param l list of elements in given feature i mentioned in the parent function.
     * @return double value mean
     */
    def calculateMean(l: List[Double]): Double = {
      val totalSum = l.reduce((a,b) => a+b)
      val lengthOfList = calculateLength(l)
      val meanAverage = totalSum / lengthOfList
      return meanAverage
    }

    /**
     * Returns standard deviation value of a list of elements in a given feature
     *
     * @param l list of elements in given feature i mentioned in the parent function.
     * @return double value standard deviation
     */
    def calculateStd(l:List[Double]):Double = {
      val mu = calculateMean(l)
      val n = calculateLength(l)

      val variance = l.map((x => math.pow(x - mu,2)))
      val varianceSum = variance.reduce((a,b) => a+b)
      val sd = math.sqrt(varianceSum / (n-1))
      return sd
    }

    /**
     * Returns length/size of a list of elements in a given feature
     *
     * @param l list of elements in given feature i mentioned in the parent function.
     * @return int value (size/length of list)
     */
    def calculateLength(l:List[Double]): Int = {
      val count = l.map(x => 1)
      val length = count.reduce((a,b) => a+b)
      return length
    }

    /**
     * Returns a list of elements in a given feature
     *
     * @param da list of list of elements i.e data.
     * @return list of elements in given feature i mentioned in the parent function.
     */
    def feList(da:List[List[Double]]):List[Double] ={
      /**
       * Returns a list of elements in a given feature
       *
       * @param d list of list of elements which is the copy of feList parameter da.
       * @param agList list of elements of the given feature.
       * @return list of double type elements.
       */
      def fList(d:List[List[Double]], agList:List[Double]):List[Double] ={
        d match {
          case Nil => agList
          case _ =>
          {
            /**
             * Returns a list of elements in a given feature
             *
             * @param l list of list of elements which belongs to head of parameter d in function fList.
             * @param l1 the double value which belongs to head of head of parameter d in function fList.
             * @param index the int value which is compared with the given feature value i.
             * @return list of double type elements.
             */
            @tailrec
            def fe(l:List[Double],l1:Double, index:Int): List[Double] = {
              l match {
                case Nil => fList(d.tail,agList:::l1::Nil)
                case h::t => if(index == i) fe(t,h, index+1)
                else fe(t,l1,index+1)
              }
            }
            fe(d.head, d.head.head,0)
          }
        }
      }
      fList(da,Nil)
    }
    val stats = Stats(calculateMin(feList(data)), calculateMax(feList(data)),
      calculateMean(feList(data)), calculateStd(feList(data)))
    return stats
  }

  /**
   * Returns a new DataFrame where the values have been standarized.
   * See exercise 2.12 (Standarization).
   */
  def standarize(): DataFrame = {
    /**
     * Returns a new standardize dataframe
     *
     * @return list of list of elements of double type
     */
    def stdList:List[List[Double]] = {
      /**
       * Returns a new standarize list of list of elements
       *
       * @param d list of list of elements i.e data.
       * @param agList new standarize list of list of elements.
       * @return list of list of elements of double type
       */
      def newStdList(d: List[List[Double]], agList: List[List[Double]]):
      List[List[Double]] = {
        d match {
          case Nil => agList
          case _ =>
          {
            /**
             * Returns list of list of elements
             *
             * @param l list of elements of double type.
             * @param l1 list of elements of double type.
             * @param i int value.
             * @return list of list of elements of double type
             */
            @tailrec
            def std(l: List[Double], l1: List[Double], i: Int): List[List[Double]] = {
              if (l.isEmpty) newStdList(d.tail, agList ::: l1 :: Nil)
              else {
                val getStats = stats(i)
                std(l.tail, l1 ::: (l.head - getStats.avg) / getStats.std :: Nil, i + 1)
              }
            }
            std(data.head, Nil, 0)
          }
        }
      }
      newStdList(data, Nil)
    }
    val frame = new DataFrame(stdList)
    return frame
  }

  /**
   * Returns the distance/dissimilarity matrix for the current data.
   * See exercises 4.4 (Minkowski) and 6.5 (Dissimilarity matrix).
   * @param measure The distance measure. It should accept "euclidean" (default)
   * and "manhattan".
   * @return A Table object (List[List[Double]]) where each row represents a the
   * distance between data point A and all other data points.
   */
  def distMatrix(measure: String = "euclidean"): Table = {
    /**
     * Returns a distance between two lists
     *
     * @param ls1 list of double elements.
     * @param ls2 list of double elements.
     * @return double value distance
     */
    def minkowski(ls1: List[Double], ls2: List[Double]): Double = {
      val distanceBetween = {
        if (measure == "euclidean") {
          if (ls1.tail.isEmpty)
            math.pow(ls1.head - ls2.head, 2)
          else
            math.pow(ls1.head - ls2.head, 2) + minkowski(ls1.tail, ls2.tail)
        }
        else {
          if (ls1.tail.isEmpty)
            (ls1.head - ls2.head).abs
          else
            (ls1.head - ls2.head).abs + minkowski(ls1.tail, ls2.tail)
        }
      }
      return distanceBetween
    }

    /**
     * Returns a list of list distance matrix
     *
     * @return list of list of double value distance
     */
    def distMat:List[List[Double]] = {
      /**
       * Returns a distance between two lists
       *
       * @param d list of list of double elements i.e data.
       * @param agList list of list double elements i.e distance.
       * @return list of list of double value distance
       */
      def getDistance(d: List[List[Double]], agList: List[List[Double]]):
      List[List[Double]] = {
        d match {
          case Nil => agList
          case _ =>
          {
            /**
             * Returns list of list of elements.
             *
             * @param l list of list of double elements i.e data.
             * @param l1 selected double value.
             * @return list of list of double value elements
             */
            @tailrec
            def dis(l: List[List[Double]], l1: List[Double]): List[List[Double]] = {
              if (l.isEmpty) getDistance(d.tail, agList ::: l1 :: Nil)
              else {dis(l.tail,
                l1:::(if(measure== "euclidean") math.sqrt(minkowski(d.head, l.head))
              else minkowski(d.head, l.head))::Nil)
              }
            }

            dis(data, Nil)
          }
        }
      }
      getDistance(data, Nil)
    }
    return distMat
  }

  /**
   * Returns a histogram for the fiven feature.
   * Note: List built-in functions allowed here.
   *
   * @param i Feature/attribute index for which the histogram is computed.
   * @param bins Number of bins (splits) into which the data is counted.
   * @return A list of tuples, where the first element of the tuple represents
   * the lower boundary of the bin, and the second the counts for elements equal
   * or aboved that boundary and lower than the next.
   *
   */
  def histogram(i: Int, bins: Int): List[(Double, Int)] = {
    /**
     * Returns list of values in a given feature with list of list.
     *
     * @param da list of list of double elements.
     * @return list of double value elements
     */
    def Feature(da: List[List[Double]]): List[Double] = {
      /**
       * Returns list of values in a given feature with list of list.
       *
       * @param d list of list of double elements.
       * @param agList list of selected double elements.
       * @return list of double value elements
       */
      def feature(d: List[List[Double]], agList: List[Double]): List[Double] = {
        d match {
          case Nil => agList
          case _ =>
          {
            /**
             * Returns list of elements.
             *
             * @param l list of double elements.
             * @param l1 selected double value.
             * @param index iterative position.
             * @return list of double value elements
             */
            @tailrec
            def fe(l: List[Double], l1: Double, index: Int): List[Double] = {
              l match {
                case Nil => feature(d.tail, agList ::: l1 :: Nil)
                case h :: t => if (index == i) fe(t, h, index + 1)
                else fe(t, l1, index + 1)
              }
            }
            fe(d.head, d.head.head, 0)
          }
        }
      }
      feature(da, Nil)
    }

    /**
     * Returns minimum value of a list of elements in a given feature
     *
     * @param l list of elements in given feature i mentioned in the parent function.
     * @return double value min
     */
    def calculateMin(l: List[Double]): Double = {
      @tailrec
      def minAcc(l: List[Double], currMin: Double): Double = {
        l match {
          case Nil => currMin
          case head :: tail =>
            val nexMin = if (head < currMin) head else currMin
            minAcc(tail, nexMin)
        }
      }
      minAcc(l, l.head)
    }

    /**
     * Returns maximum value of a list of elements in a specified feature
     *
     * @param l list of elements in specified feature i mentioned in the parent function.
     * @return double value max
     */
    def calculateMax(l: List[Double]): Double = {
      @tailrec
      def maxAcc(l: List[Double], currMax: Double): Double = {
        l match {
          case Nil => currMax
          case head :: tail =>
            val nexMax = if (head > currMax) head else currMax
            maxAcc(tail, nexMax)
        }
      }
      maxAcc(l, l.head)
    }

    val min = calculateMin(Feature(data))
    val max = calculateMax(Feature(data))
    val ran = ((max - min) / bins)

    /**
     * Returns filtered list on given conditions
     *
     * @param list list  of given feature values.
     * @param range1 double value of lower bound.
     * @param range2 double value of upper bound.
     * @param filt list of a new filtered values.
     * @return list of double values
     */
    def filter(list: List[Double], range1: Double, range2: Double,
               filt: List[Double] = Nil): List[Double] = {
      if (list.isEmpty) filt
      else {
        if (list.head >= range1 &&  list.head < range2)
          filter(list.tail, range1, range2, filt ::: list.head :: Nil)
        else filter(list.tail, range1, range2, filt)
      }
    }

    /**
     * Returns length/size or total number of rows
     *
     * @param table list of elements in a given feature.
     * @return int value (size/length/number of elements  of a list)
     */
    def count(table: List[Double], c: Int=0): Int = {
      table match {
        case Nil => c
        case _ :: t => count(t, c + 1)
      }
    }

    /**
     * Returns list of a tuple values
     *
     * @param histList list of tuple containing lower value and number of elements.
     * @param bin position of the list ranges.
     * @return list of tuple values
     */
    def hs(histList:List[(Double,Int)] = Nil, bin:Int = 0):List[(Double,Int)] =
      {
        if(bin==bins) histList
        else {
          val currentStart = ((bin) * ran) + min //Bottom
          val currentEnd = ((bin) * ran) + min + ran //UP
            val currentList =
              if(bin==bins-1) filter(Feature(data), currentStart, currentEnd+ran)
          else filter(Feature(data), currentStart, currentEnd)
          hs(histList:::(if(currentList==Nil ) currentStart
          else calculateMin(currentList),count(currentList))::Nil,bin+1)
        }

      }
    return hs()
  }
  /**
   * Displays the first 10 rows in console.
   */
  def show(): Unit = data.take(10).map(_.mkString("\t| ")).foreach(println)

  /**
   * Displays a simple plot for the histrogram in console.
   * @param i Feature/attribute index for which the histogram is to be computed.
   * @param bins Number of bins (splits) into which the data is counted.
   */
  def plotHistogram(i: Int, bins: Int): Unit =
    histogram(i, bins).map{case (i, c) =>
      "> %2.2f ".format(i) +
        (1 to c).map(_ => "-").mkString("|", "", "o")
    }.foreach(println)
}

/**
 * This companion object will be the entry point. Users may load datasets using
 * the read function, e.g.:
 *   val df: DataFrame = DataFrame.read("path/to/file/data.csv")
 */
object DataFrame extends App {
  /**
   * Loads and returns a DataFrame for the given data file. It should accept CSV
   * (comma separated value) files with numeric data only.
   * Note: Built-in functions allowed here. Check scala.io.Source for reading files.
   *
   * @param path Full path to data file.
   * @param sep  (Optional) The separation character between columns in the file.
   * @return An Either object where the Left represents a DataFrame (when
   *         loaded correctly), and the Right a String (an error message in case the
   *         loading failed, e.g., no numeric values).
   */
  def read(path: String, sep: String = ","): Either[DataFrame, String] = {
    import java.io.FileNotFoundException
    try {
      val source = Source.fromFile(path)
      val lines = source.getLines()
      val data = lines.map(_.split(sep).map(_.toDouble).toList).toList
      val frame = new DataFrame(data)
      if(data.isEmpty) return Right("The file is empty.")

      Left(frame)
    }
    catch {
      case a: NumberFormatException =>
        Right("Conversion problem, the file consists of non-numeral values.  " + a)
      case b:FileNotFoundException =>
        Right("The file does not exist.  " + b)
    }
  }
}
