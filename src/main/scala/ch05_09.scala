object Ch05_09 {

  case class Book(title: String, authors: List[String])

  val books = List(
    Book("FP in Scala", List("Chiusano", "Bjarnason")),
    Book("The Hobbit", List("Tolkien")))

  case class Movie(title: String)

  def bookAdaptations(author: String): List[Movie] =
    if (author == "Tolkien")
      List(Movie("An Unexpected Journey"),
           Movie("The Desolation of Smaug"))
    else List.empty


  def run(): Unit = {
    {
      val movies = books.flatMap(_.authors).flatMap(bookAdaptations)
      println(movies)
      assert(movies == List(Movie("An Unexpected Journey"),
                            Movie("The Desolation of Smaug")))
    }
    {
      val recommendedFeed = books.flatMap(book =>
        book.authors.flatMap(author =>
          bookAdaptations(author).map(movie =>
            s"You may like ${movie.title} because you liked ${author}'s ${book.title}"
          )))
      println(recommendedFeed)
    }
    {
      val recommendedFeed = for {
        book <- books
        author <- book.authors
        movie <- bookAdaptations(author)
      } yield s"You may like ${movie.title} because you liked ${author}'s ${book.title}"
      println(recommendedFeed)
    }

  }
}
