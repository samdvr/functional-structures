trait Applicative[F[_]] {
  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))
}


object Applicative {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case (None, _) => None
      case (Some(a), None) => None
      case (Some(a), Some(f)) => Some(f(a))

    }

    override def pure[A](a: A): Option[A] = Some(a)
  }
}