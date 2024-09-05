object Ch04_50 {

  def largerThanNumbers(n: Int)(numbers: List[Int]): List[Int] = {
    numbers.filter(i => i > n)
  }

  def divByNumbers(n: Int)(numbers: List[Int]): List[Int] = {
    numbers.filter(i => i % n == 0)
  }

  def lessThanWords(n: Int)(words: List[String]): List[String] = {
    words.filter(w => w.length < n)
  }

  def containsSCountWords(n: Int)(words: List[String]): List[String] = {
    words.filter(w => w.length - w.replaceAll("s", "").length >= n)
  }

  def run(): Unit = {
    {
      val f: List[Int] => List[Int] = largerThanNumbers(4)
      println(f(List(5, 1, 2, 4, 0)))
      assert(f(List(5, 1, 2, 4, 0)) == List(5))
    }
    {
      val f: List[Int] => List[Int] = divByNumbers(5)
      println(f(List(5, 1, 2, 4, 15)))
      assert(f(List(5, 1, 2, 4, 15)) == List(5, 15))
    }
    {
      val f: List[String] => List[String] = lessThanWords(4)
      println(f(List("scala", "ada")))
      assert(f(List("scala", "ada")) == List("ada"))
    }
    {
      val f: List[String] => List[String] = containsSCountWords(2)
      println(f(List("scala", "ada")))
      assert(f(List("scala", "ada")) == List.empty)
    }

//    val list2 = List(5, 1, 2, 4, 15)
//    val result2: Int => List[Int] = div => list2.filter(n => n % div == 0)
//
//    println(result2(5))
//    assert(result2(5) == List(5, 15))
//
//    println(result2(2))
//    assert(result2(2) == List(2, 4))
//
//    val list3 = List("scala", "ada")
//    val result3: Int => List[String] = len => list3.filter(word => word.length < len)
//
//    println(result3(5))
//    assert(result3(5) == List("ada"))
//
//    println(result3(7))
//    assert(result3(7) == List("scala", "ada"))
//
//    val list4 = List("rust", "ada")
//    val result4: Int => List[String] = count => list4.filter(word => word.filter(w => w == 's').length >= count)
//
//    println(result4(3))
//    assert(result4(3) == List.empty)
//
//    println(result4(1))
//    assert(result4(1) == List("rust"))
  }
}
