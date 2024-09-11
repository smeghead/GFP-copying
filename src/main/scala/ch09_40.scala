
import cats.effect.IO
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global
import scala.util.Random

object Ch09_40 {

  def castTheDieImpure(): Int = {
    println("castTheDieImpure")
    val r = new Random
    r.nextInt(6) + 1
  }

  def castTheDie(): IO[Int] = IO.delay(castTheDieImpure())
 
  val infiniteDieCasts: Stream[IO, Int] = Stream.eval(castTheDie()).repeat

  def run(): Unit = {
    {
      val result: IO[List[Int]] = infiniteDieCasts.take(5).compile.toList
      println(result.unsafeRunSync())
    }
    {
      val result: IO[List[Int]] = infiniteDieCasts.filter(_ % 2 == 1).take(3).compile.toList
      println(result.unsafeRunSync())
    }
    {
      val result: IO[List[Int]] = infiniteDieCasts.take(5).map(n => if (n == 6) n * 2 else n).compile.toList
      println(result.unsafeRunSync())
    }
    {
      val result: IO[Int] = infiniteDieCasts.take(3).compile.toList.map(_.sum)
      println(result.unsafeRunSync())
    }
    {
      val result: IO[List[Int]] = infiniteDieCasts.filter(_ == 5).take(1)
        .append(infiniteDieCasts.take(2)).compile.toList
      println(result.unsafeRunSync())
    }
    {
      val result: IO[List[Int]] = infiniteDieCasts.take(3).append(
        infiniteDieCasts.take(3).map(_ * 3)
      ).compile.toList
      println(result.unsafeRunSync())
    }
    {
      val result: IO[List[Int]] = infiniteDieCasts.scan(0)((sixesInRow, current) =>
        if (current == 6) sixesInRow + 1 else 0).filter(_ == 2).take(1).compile.toList
      println(result.unsafeRunSync())
    }
  }
}
