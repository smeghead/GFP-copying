object Ch03_19 {
  def firstTwo(items: List[String]): List[String] = items.take(2)

  def lastTwo(items: List[String]): List[String] = items.takeRight(2)

  def moveFirstTwoToTyeEnd(items: List[String]): List[String] = {
    val first = firstTwo(items)
    items.slice(2, items.size).appendedAll(first)
  }

  def insertedBeforeLast(list: List[String], element: String): List[String] = {
    val last = list.takeRight(1)
    list.take(list.size - 1).appended(element).appendedAll(last)
  }
}
