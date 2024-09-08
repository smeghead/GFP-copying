
object Ch07_09 {

  enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
  }
  import MusicGenre._

  enum YearsActive {
    case StillActive(since: Int)
    case ActiveBetween(start: Int, end: Int)
  }
  import YearsActive._

  def wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int): Boolean = {
    artist.yearsActive match {
      case StillActive(since)        => since <= yearEnd
      case ActiveBetween(start, end) => start <= yearEnd && end >= yearStart
    }
  }

  opaque type Location = String
  object Location {
    def apply(value: String): Location = value
    extension(a: Location) def name: String = a
  }

  case class Artist(
    name: String,
    genre: MusicGenre,
    origin: Location,
    yearsActive: YearsActive
  )

  def searchArtists(
    artists: List[Artist],
    genres: List[MusicGenre],
    locations: List[String],
    searchByActiveYears: Boolean,
    activeAfter: Int,
    activeBefore: Int
  ): List[Artist] = {
    artists.filter(artist =>
      (genres.isEmpty || genres.contains(artist.genre)) &&
      (locations.isEmpty || locations.contains(artist.origin)) &&
      (searchByActiveYears || wasArtistActive(artist, activeAfter, activeBefore)))
  }

  def activeLength(artist: Artist, currentYear: Int): Int = {
    artist.yearsActive match {
      case StillActive(since) => currentYear - since
      case ActiveBetween(start, end) => end - start
    }
  }

  val artists = List(
    Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981)),
    Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)),
    Artist("Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003))
  )

  def run(): Unit = {
    {
      val result = searchArtists(artists, List(Pop), List("England"), true, 1950, 2022)
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
    println("7.29 Pattern Matching")
    {
      val result = activeLength(Artist(
        "Metallica",
        HeavyMetal,
        Location("U.S."),
        StillActive(1981)), 2022)
      println(result)
      assert(result == 41)
    }
    {
      val result = activeLength(Artist(
        "Led Zeppelin",
        HardRock,
        Location("England"),
        ActiveBetween(1968, 1980)), 2022)
      println(result)
      assert(result == 12)
    }
    {
      val result = activeLength(Artist(
        "Bee Gees", Pop, Location("England"), ActiveBetween(1958, 2003)), 2022)
      println(result)
      assert(result == 45)
    }
  }
}
