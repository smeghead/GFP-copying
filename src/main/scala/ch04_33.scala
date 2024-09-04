object Ch04_33 {

  def run(): Unit = {
    val list1 = List("scala", "rust", "ada")
    var result = list1.filter(s => s.length < 5)
    println(result)
    assert(result == List("rust", "ada"))

    val list2 = List("rust", "ada")
    result = list2.filter(s => s.filter(c => c == 's').length >= 3)
    println(result)
    assert(result == List.empty)

    val list3 = List(5, 1, 2, 4, 0)
    var result2 = list3.filter(n => n % 2 == 1)
    println(result2)
    assert(result2 == List(5, 1))

    result2 = list3.filter(n => n > 4)
    println(result2)
    assert(result2 == List(5))
  }
}
