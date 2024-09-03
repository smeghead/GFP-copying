object Ch04_17 {
  def len(s: String): Int = s.length

  def countS(s: String): Int = s.filter(str => str == 's').length

  def desc(n: Int): Int = n * -1

  def run() = {
    val list1 = List("scala", "rust", "ada")
    println(list1.sortBy(len))
    assert(list1.sortBy(len) == List("ada", "rust", "scala"))

    val list2 = List("rust", "ada")
    println(list2.sortBy(countS))
    assert(list2.sortBy(countS) == List("ada", "rust"))

    val list3 = List(5, 1, 2, 4, 3)
    println(list3.sortBy(desc))
    assert(list3.sortBy(desc) == List(5, 4, 3, 2, 1))

    println(list2.sortBy(s => countS(s) * -1))
    assert(list2.sortBy(s => countS(s) * -1) == List("rust", "ada"))
  }
}
