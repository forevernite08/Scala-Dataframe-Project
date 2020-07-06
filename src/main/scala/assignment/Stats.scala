package assignment

/**
 * A simple class for wrapping some statistics. Used in by the stats method of
 * the DataFrame class.
 *
 * @param min the minimum value in a list.
 * @param max the maximum value in a list.
 * @param avg the average value of elements in a list.
 * @param std the standard deviation of the elements in a list.
 */
case class Stats(min: Double, max: Double, avg: Double, std: Double)

