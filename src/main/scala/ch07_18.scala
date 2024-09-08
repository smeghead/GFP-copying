
object Ch07_18 {

  case class User(name: String, city: Option[String], favoriteArtists: List[String])

  val users = List(
    User("Alice", Some("Melbourne"), List("Bee Gees")),
    User("Bob", Some("Lagos"), List("Bee Gees")),
    User("Eve", Some("Tokyo"), List.empty),
    User("Mallory", None, List("Metallica", "Bee Gees")),
    User("Trent", Some("Buenos Aires"), List("Led Zeppelin"))
  )

  def run(): Unit = {
    println(users.filter(_.city.forall(_ == "Melbourne")))
    println(users.filter(_.city.contains("Lagos")))
    println(users.filter(_.favoriteArtists.contains("Bee Gees")))
    println(users.filter(_.city.exists(_.startsWith("T"))))
    println(users.filter(_.favoriteArtists.forall(_.length > 8)))
    println(users.filter(_.favoriteArtists.exists(_.startsWith("M"))))
  }
}
