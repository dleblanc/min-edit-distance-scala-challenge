import java.util.regex.{Matcher, Pattern}

/**
 * Heap Analytics take-home assignment - Dave LeBlanc, david.leblanc@gmail.com
 *
 * Looking at the problem, it appears that this is related to an edit-distance problem.
 *
 * I started by parsing out the Dom Hierarchy strings (via regex), and decomposing them into a class structure.
 *
 * Next up I tackled the edit distance problem using a naive Levenshtein Distance calculation - but since one of the
 * test strings is very long, this was taking too long to run. I then switched to a dynamic programming approach.
 *
 * For the ids - I just did a pairwise comparison on which ids needed inserts - in retrospect this is probably too simple
 * in the case where the number of elements is different.
 *
 * I took a similar approach with the classes - but since there's multiple, I did a set difference operation to count the
 * number of changes that needed to be made.
 *
 * All in all, I wasted a bunch of time fiddling with regexes and trying to get the DP stuff working just right.
 *
 *
 * For running this example - you need Java 7 (I believe) - and all the additional files to run it should be included.
 *
 * To build and run the tests:
 *
 *   ./activator test
 *
 *
 * Update: Got interested in how to do it properly. Did a per-tag (element) add/del/swap cost function, switched to
 * memoization from dynamic programming, and chipped away at the test cases until it all worked. Took surprisingly long
 * for a two-hour programming challenge (if that's just me, I don't know).
 */

object DomHierarchy {

  def fromString(domStr: String): DomHierarchy = {
    DomHierarchy(domStr
      .split(" ")
      .toList
      .map(Element.parseElement)
    )
  }

  def domHierarchyLevDist(first: Seq[Element], second: Seq[Element]): Int = {
    val swapCostFun = (first: Element, second: Element) => if (first == second) 0 else first.getSwapCostTo(second)
    val addCostFun = (item: Element) => item.getAddCost
    val delCostFun = (item: Element) => item.getDelCost

    MinEditDistance.getMinEditDistance(first, second, swapCostFun, addCostFun, delCostFun)
  }

}

case class DomHierarchy(elements: Seq[Element])