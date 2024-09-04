object Ch04_22 {
  def rankedWords(wordScore: String => Int, words: List[String]): List[String] = {
    words.sortBy(wordScore).reverse
  }

  def score(word: String): Int = word.replaceAll("a", "").length

  def bonus(word: String): Int = if (word.contains("c")) 5 else 0

  def penalty(word: String): Int = if (word.contains("s")) -7 else 0


  def run() = {
    val list1 = List("scala", "rust", "ada")
    println(rankedWords(word => score(word), list1))
    assert(rankedWords(word => score(word), list1) == List("rust", "scala", "ada"))

    println(rankedWords(word => score(word) + bonus(word), list1))
    assert(rankedWords(word => score(word) + bonus(word), list1) == List("scala", "rust", "ada"))

    println(rankedWords(word => score(word) + bonus(word) + penalty(word), list1))
    assert(rankedWords(word => score(word) + bonus(word) + penalty(word), list1) == List("ada", "scala", "rust"))
  }
}
