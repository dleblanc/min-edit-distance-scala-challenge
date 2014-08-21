
object StrLev {

  def strLevDist(source: String, target: String): Int = {
    val swapCostFun = (first: Char, second: Char) => if (first == second) 0 else 1
    val addCostFun = (item: Char) => 1
    val delCostFun = (item: Char) => 1

    MinEditDistance.getMinEditDistance(source, target, swapCostFun, addCostFun, delCostFun)
  }

}
