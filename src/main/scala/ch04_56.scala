object Ch04_56 {

  def run(): Unit = {
    {
      val result  = List(5, 1, 2, 4, 100).foldLeft(0)((total, n) => total + n)
      println(result)
      assert(result == 112)
    }
    {
      val result  = List("scala", "rust", "ada").foldLeft(0)((total, word) => total + word.length)
      println(result)
      assert(result == 12)
    }
    {
      val result  = List("scala", "haskell", "rust", "ada").foldLeft(0)((total, word) => total + word.length - word.replaceAll("s", "").length)
      println(result)
      assert(result == 3)
    }
    {
      val result  = List(5, 1, 2, 4, 15).foldLeft(Int.Minvalue)((max, n) => if (max < n) n else max)
      println(result)
      assert(result == 15)
    }
    {
      val result  = List(5, 1, 2, 4, 15).foldLeft(Int.Minvalue)(Math.max)
      println(result)
      assert(result == 15)
    }

  }
}
