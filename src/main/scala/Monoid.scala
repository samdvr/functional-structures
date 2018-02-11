trait Monoid[A] {
  def append(a: A, b: A): A

  def identity: A
}

object IntegerAddition extends Monoid[Int] {
  def append(a: Int, b: Int): Int = a + b

  def identity: Int = 0
}

object StringAddition extends Monoid[String] {
  def append(a: String, b: String): String = a + b

  def identity: String = ""
}