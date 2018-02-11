trait Monad[M[_]] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](a: M[A])(fn: A => M[B]): M[B]

  def map[A, B](a: M[A])(fn: A => B): M[B] = {
    flatMap(a) { b: A => pure(fn(b)) }
  }
}

object Monad {
  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](a: List[A])(fn: A => List[B]): List[B] = a.flatMap(fn)
  }
}