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

  }
}
