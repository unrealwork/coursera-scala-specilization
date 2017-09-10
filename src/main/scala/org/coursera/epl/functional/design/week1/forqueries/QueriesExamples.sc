/**
  * Case classes are like regular classes with a few key
  * differences which we will go over. Case classes
  * are good for modeling immutable data.
  * In the next step of the tour, we’ll see how they are
  * useful in pattern matching.
  *
  * Class Book
  *
  * @param title
  * @param authors
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
for (b <- books; a <- b.authors if (a startsWith "Bird,"))
  yield b.title

/* To find all the books which have the word “Program” in the title. */

for (b <- books; if ((b.title indexOf "Program") >= 0))
  yield b.title

/* To find the names of all authors who have written at least two
books present in the database. */

for {
  b1 <- books
  b2 <- books
  if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1

// To avoid duplicates: use lexicographical order
{
  for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
}.distinct

