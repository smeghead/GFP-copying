object Ch04_28 {

  def run() = {
    val list1 = List("scala", "rust", "ada")
    var result = list1.map(s => s.length)
    println(result)
    assert(result == List(5, 4, 3))

    val list2 = List("rust", "ada")
    result = list2.map(s => s.filter(c => c == 's').length)
    println(result)
    assert(result == List(1, 0))

    val list3 = List(5, 1, 2, 4, 0)
    result = list3.map(n => n * -1)
    println(result)
    assert(result == List(-5, -1, -2, -4, 0))

    result = list3.map(n => n * 2)
    println(result)
    assert(result == List(10, 2, 4, 8, 0))
  }
}
