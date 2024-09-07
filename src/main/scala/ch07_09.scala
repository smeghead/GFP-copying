
object Ch07_09 {

  case class Location(name: String)
  case class Genre(name: String)
  case class YearsActiveStart(value: Int)
  case class YearsActiveEnd(value: Int)

  // newtype がエラーになる。
  // opaque type Location = String
  // object Location {
  //   def apply(value: String): Location = value
  //   extension(a: Location) def name: String = a
  // }

  case class Artist(
    name: String,
    genre: Genre,
    origin: Location,
    yearActiveStart: YearsActiveStart,
    isActive: Boolean,
    yearActiveEnd: YearsActiveEnd
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
                        artists.filter(artist => genres.contains(artist.genre.name))
                      else artists
    val locationResult = if (locations.length > 0)
                           genreResult.filter(artist => locations.contains(artist.origin.name))
                         else genreResult
    if (searchByActiveYears)
      locationResult.filter(artist => artist.yearActiveStart.value <= activeBefore &&
                                        (artist.isActive || artist.yearActiveEnd.value >= activeAfter))
                                        else locationResult
  }

  val artists = List(
    Artist("Metallica", Genre("Heave Metal"), Location("U.S."), YearsActiveStart(1981), true, YearsActiveEnd(0)),
    Artist("Led Zeppelin", Genre("Hard Rock"), Location("England"), YearsActiveStart(1968), false, YearsActiveEnd(1980)),
    Artist("Bee Gees", Genre("Pop"), Location("England"), YearsActiveStart(1958), false, YearsActiveEnd(2003))
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
