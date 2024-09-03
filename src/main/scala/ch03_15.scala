object Ch02_15 {
  def abbreviate(str: String): String = {
    val parts = str.split(" ")
    val firstName = parts(0)
    val lastName = parts(1)
    val abbr =
      if (firstName.length == 1) firstName
      else firstName.substring(0, 1)
    return String.format("%s. %s", abbr, lastName)
  }
}
