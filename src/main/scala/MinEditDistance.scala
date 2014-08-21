
/**
 * An implementation of the Levenshtein algorithm for finding minimum edit distance. Uses memoization and a
 * parameterized variable-cost approach.
 */
object MinEditDistance {

  def getMinEditDistance[T](
                              source: Seq[T],
                              target: Seq[T],
                              swapCostFun: (T, T) => Int,
                              addCostFun: (T) => Int,
                              delCostFun: (T) => Int,
                              cache: scala.collection.mutable.Map[(Seq[T], Seq[T]), Int] = scala.collection.mutable.Map.empty[(Seq[T], Seq[T]), Int]
                              ): Int = {
    if (source.length <= 0) {

      target.map(addCostFun).sum

    } else if (target.length <= 0) {

      source.map(delCostFun).sum

    } else {

      def cachedGet(first: Seq[T], second: Seq[T], cost: => Int) = {
        cache.getOrElseUpdate((first, second), getMinEditDistance(first, second, swapCostFun, addCostFun, delCostFun, cache)) + cost
      }

      Seq(
        cachedGet(source.tail, target.tail, swapCostFun(source.head, target.head)),                           // swap
        cachedGet(source.tail, target, delCostFun(source.head)),                                              // deletion
        cachedGet(source, target.tail, addCostFun(target.head))                                               // insertion
      ).min

    }
  }
}
