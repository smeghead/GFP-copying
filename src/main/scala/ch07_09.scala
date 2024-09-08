
object Ch07_09 {

  enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
  }
  import MusicGenre._

  case class Location(name: String)

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

  // // newtype がエラーになる。
  // opaque type Location = String
  // object Location {
  //   def apply(value: String): Location = value
  //   extension(a: Location) def name: String = a
  // }

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
    val genreResult = if (genres.length > 0)
                        artists.filter(artist => genres.contains(artist.genre))
                      else artists
    val locationResult = if (locations.length > 0)
                           genreResult.filter(artist => locations.contains(artist.origin.name))
                         else genreResult
    if (searchByActiveYears)
      locationResult.filter(artist => wasArtistActive(artist, activeAfter, activeBefore))
      //  artist.yearsActive.start <= activeBefore &&
      //                                (artist.yearsActive.end.forall(_ >= activeAfter))
    else locationResult
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

  }
}
