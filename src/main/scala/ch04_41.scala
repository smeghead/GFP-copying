object Ch04_41 {


  def run(): Unit = {
    val list1 = List(5, 1, 2, 4, 0)
    val result1: Int => List[Int] = largerThan => list1.filter(n => n > largerThan)
    println(result1(4))
    assert(result1(4) == List(5))

    println(result1(1))
    assert(result1(1) == List(5, 2, 4))

    val list2 = List(5, 1, 2, 4, 15)
    val result2: Int => List[Int] = div => list2.filter(n => n % div == 0)

    println(result2(5))
    assert(result2(5) == List(5, 15))

    println(result2(2))
    assert(result2(2) == List(2, 4))

    val list3 = List("scala", "ada")
    val result3: Int => List[String] = len => list3.filter(word => word.length < len)

    println(result3(5))
    assert(result3(5) == List("ada"))

    println(result3(7))
    assert(result3(7) == List("scala", "ada"))

    val list4 = List("rust", "ada")
    val result4: Int => List[String] = count => list4.filter(word => word.filter(w => w == 's').length >= count)

    println(result4(3))
    assert(result4(3) == List.empty)

    println(result4(1))
    assert(result4(1) == List("rust"))
  }
}
