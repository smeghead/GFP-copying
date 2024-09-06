object Ch05_12 {

  case class Point(x: Int, y: Int)

  def run(): Unit = {
    {
      val points = List(1).flatMap(x =>
        List(-2, 7).map(y =>
          Point(x, y)
        )
      )
      println(points)
      assert(points == List(Point(1, -2),
                            Point(1, 7)))
    }
  }
}
