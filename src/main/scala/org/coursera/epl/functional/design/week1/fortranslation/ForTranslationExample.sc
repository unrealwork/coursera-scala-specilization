
def isPrime(i: Int): Boolean = {
  val sqrtCeil = Math.sqrt(i).ceil.toInt + 1
  (2 until sqrtCeil).forall(n => i % n != 0)
}

/**
  * Take the for-expression that computed pairs whose sum is prime
  *
  * @param n up limit of numbers in generated pairs
  * @return
  */
def forPrimePairs(n: Int): Seq[(Int, Int)] = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

/**
  * Applying the translation scheme to this expression gives.
  * @param n
  * @return
  */
def translatedPrimePairs(n: Int): Seq[(Int, Int)] = {
  (1 until n).flatMap(i =>
    (1 until i)
      .withFilter(j => isPrime(i + j))
      .map(j => (i, j))
  )
}

forPrimePairs(4) == translatedPrimePairs(4)

/*
Translate
  for (b <- books; a <- b.authors if a startsWith ”Bird”)
    yield b.title
  into higher-order functions.
 */

case class Book(title: String, authors: List[String])

val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Effective Java 1",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

/* To find the titles of books whose author’s name is “Bird” */
for (b <- books; a <- b.authors if a startsWith "Bird,")
  yield b.title

/**
  * Translated
  */
books.flatMap(b =>
  b.authors
    .withFilter(a => a startsWith "Bird,")
    .map(a => b.title)
)