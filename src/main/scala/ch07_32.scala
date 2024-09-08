
object Ch07_32 {

  enum MusicGenre {
    case Funk
    case House
    case Rock
  }
  import MusicGenre._

  opaque type Artist = String
  object Artist {
    def apply(value: String): Artist = value
    extension(a: Artist) def name: String = a
  }

  opaque type User = String
  object User {
    def apply(value: String): User = value
    extension(a: User) def name: String = a
  }

  enum PlaylistType {
    case CustomSongsList(name: User)
    case ArtistSongsList(artist: Artist)
    case GenreSongsList(genres: List[MusicGenre])
  }
  import PlaylistType._

  case class Song(name: String, artist: Artist)

  case class Playlist(name: String, playlistType: PlaylistType, songs: List[Song])

  def gatherSongs(playlists: List[Playlist], artist: Artist, genre: MusicGenre): List[Song] = {
    playlists.flatMap(playlist =>
      playlist.playlistType match {
        case CustomSongsList(name) => playlist.songs.filter(_.artist == artist)
        case ArtistSongsList(a) => if (a == artist) playlist.songs else List.empty
        case GenreSongsList(genres) => if (genres.contains(genre)) playlist.songs else List.empty
      }
    )
  }

  val playlists = List(
    Playlist(
      "This is Foo Fighters",
      ArtistSongsList(Artist("Foo Fighters")),
      List(
        Song("Breakout", Artist("Foo Fighters")),
        Song("Learn To Fly", Artist("Foo Fighters"))
      )),
    Playlist(
      "Deep Focus",
      GenreSongsList(List(Funk, House)),
      List(
        Song("One More Time", Artist("Daft Punk")),
        Song("Hey Boy Hey Girl", Artist("Chemical Brothers"))
      )
    ),
    Playlist(
      "User's",
      CustomSongsList(User("me")),
      List(
        Song("Hello, good by", Artist("The Beatles")),
        Song("Jumping Jack Flash", Artist("The Rolling Stones")),
        Song("Monkey Wrench", Artist("Foo Fighters"))
      )
    )
  )

  def run(): Unit = {
    {
      val result = gatherSongs(playlists, Artist("Foo Fighters"), Rock)
      println("Foo Fighters, Rock")
      println(result)
    }
    {
      val result = gatherSongs(playlists, Artist("Who"), Funk)
      println("Who, Funk")
      println(result)
    }
  }
}
