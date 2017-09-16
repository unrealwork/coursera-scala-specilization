import scala.util.Random

/**
  * Let’s define a trait Generator[T]
  * that generates random values of type T:
  *
  * @tparam T
  */
trait Generator[+T] {
  self =>

  def generate(): T

  def map[S](f: T => S) = new Generator[S] {
    override def generate() = f(self.generate())
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate() = f(self.generate()).generate
  }
}

/*
Some instances
 */
val integers = new Generator[Int] {
  val random = new Random()

  override def generate() = random.nextInt()
}

/**
  * Boolean
  */
val booleans = new Generator[Boolean] {
  override def generate() = integers.generate() > 0
}

/**
  * Pairs generator
  */
val pairs = new Generator[(Int, Int)] {
  override def generate() = (integers.generate(), integers.generate())
}


def pairs[T, U](t: Generator[T], u: Generator[U]) =
  t flatMap (x => u map (y => (x, y)))

pairs(integers, integers).generate()
booleans.generate()

/*
Single
 */

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

single(1).generate()

choose(1,3).generate()

oneOf(1, 3, 7).generate()