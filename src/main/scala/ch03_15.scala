object Ch02_15 {
  def abbreviate(str: String): String = {
    val separaterIndex = str.indexOf(" ")
    val firstName = str.substring(0, separaterIndex)
    return String.format("%s. %s", firstName.substring(0, 1), str.substring(separaterIndex + 1))
  }
}
