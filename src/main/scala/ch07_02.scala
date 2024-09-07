object Ch07_02 {

  case class Artist(
    name: String,
    genre: String,
    origin: String,
    yearActiveStart: Int,
    isActive: Boolean,
    yearActiveEnd: Int
  )

  def searchArtists(
    artists: List[Artist],
    genres: List[String],
    locations: List[String],
    searchByActiveYears: Boolean,
    activeAfter: Int,
    activeBefore: Int
  ): List[Artist] = {
    val genreResult = if (genres.length > 0)
                        artists.filter(artist => genres.contains(artist.genre))
                      else artists
    val locationResult = if (locations.length > 0)
                           genreResult.filter(artist => locations.contains(artist.origin))
                         else genreResult
    if (searchByActiveYears)
      locationResult.filter(artist => artist.yearActiveStart <= activeBefore &&
                                        (artist.isActive || artist.yearActiveEnd >= activeAfter))
                                        else locationResult
  }

  val artists = List(
    Artist("Metallica", "Heave Metal", "U.S.", 1981, true, 0),
    Artist("Led Zeppelin", "Hard Rock", "England", 1968, false, 1980),
    Artist("Bee Gees", "Pop", "England", 1958, false, 2003)
  )

  def run(): Unit = {
    {
      val result = searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022)
      println(result)
    }
    {
      val result = searchArtists(artists, List.empty, List("England"), true, 1950, 2022)
      println(result)
    }
    {
      val result = searchArtists(artists, List.empty, List.empty, true, 1981, 2003)
      println(result)
    }
    {
      val result = searchArtists(artists, List.empty, List("U.S."), false, 0, 0)
      println(result)
    }
    {
      val result = searchArtists(artists, List.empty, List.empty, false, 2019, 2022)
      println(result)
    }

  }
}
