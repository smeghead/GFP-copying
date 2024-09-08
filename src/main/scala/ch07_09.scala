
object Ch07_09 {

  opaque type Location = String
  object Location {
    def apply(value: String): Location = value
    extension(a: Location) def name: String = a
  }

  enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
  }
  import MusicGenre._

  enum YearsActive {
    case StillActive(since: Int)
    case ActiveBetween(start: Int, end: Int)
    case ActiveBetweenWithSuspended(start: Int, suspend: Int, restart: Int, end: Int)
  }
  import YearsActive._

  case class Artist(
    name: String,
    genre: MusicGenre,
    origin: Location,
    yearsActive: YearsActive
  )

  enum SerachCondition {
    case SearchByGenre(genres: List[MusicGenre])
    case SearchByOrigin(locations: List[Location])
    case SearchByActiveYears(start: Int, end: Int)
    case SearchByActiveTerm(term: Int, currentYear: Int)
  }
  import SerachCondition._

  def searchArtists(
    artists: List[Artist],
    requiredConditions: List[SerachCondition]
  ): List[Artist] = {
    artists.filter(artist =>
      requiredConditions.forall(condition =>
        condition match {
          case SearchByGenre(genres) => genres.contains(artist.genre)
          case SearchByOrigin(locations) => locations.contains(artist.origin)
          case SearchByActiveYears(start, end) => wasArtistActive(artist, start, end)
          case SearchByActiveTerm(term, currentYear) => activeLength(artist, currentYear) >= term
        }
      )
    )
  }

  def activeLength(artist: Artist, currentYear: Int): Int = {
    artist.yearsActive match {
      case StillActive(since) => currentYear - since
      case ActiveBetween(start, end) => end - start
      case ActiveBetweenWithSuspended(start, suspend, restart, end) => suspend - start + end - restart
    }
  }

  def wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int): Boolean = {
    artist.yearsActive match {
      case StillActive(since)        => since <= yearEnd
      case ActiveBetween(start, end) => start <= yearEnd && end >= yearStart
      case ActiveBetweenWithSuspended(start, suspend, restart, end) => {
        (start <= yearEnd && suspend >= yearStart) ||
        (restart <= yearEnd && end >= yearStart)
      }
    }
  }

  val artists = List(
    Artist("Metallica", HeavyMetal, Location("U.S."), StillActive(1981)),
    Artist("Led Zeppelin", HardRock, Location("England"), ActiveBetween(1968, 1980)),
    Artist("Bee Gees", Pop, Location("England"), ActiveBetweenWithSuspended(1958, 2003, 2009, 2012))
  )

  def run(): Unit = {
    {
      val result = searchArtists(artists, List(
        SearchByGenre(List(Pop)),
        SearchByOrigin(List("England")),
        SearchByActiveYears(1950, 2022)
      ))
      println(result)
    }
    {
      val result = searchArtists(artists, List(
        SearchByOrigin(List("England")),
        SearchByActiveYears(1950, 2022)
      ))
      println(result)
    }
    {
      val result = searchArtists(artists, List(
        SearchByActiveYears(1981, 2003)
      ))
      println(result)
    }
    {
      val result = searchArtists(artists, List(
        SearchByOrigin(List("U.S."))
      ))
      println(result)
    }
    {
      val result = searchArtists(artists, List.empty)
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
    println("7.37 change spacification")
    {
      val result = activeLength(Artist(
        "Bee Gees", Pop, Location("England"), ActiveBetweenWithSuspended(1958, 2003, 2009, 2012)), 2022)
      println(result)
      assert(result == 48)
    }
    {
      val result = searchArtists(artists, List(
        SearchByActiveTerm(30, 2024)
      ))
      println(result)
    }
    {
      val result = searchArtists(artists, List(
        SearchByActiveTerm(46, 2024)
      ))
      println(result)
    }
  }
}
