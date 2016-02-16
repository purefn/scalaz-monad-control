package scalaz.monad.control

import scalaz._
import scalaz.effect._
import scalaz.std.function._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.stream._

trait MonadBaseControl[B[_], M[_]] {
  /** Monadic state of m. */
  type StM[A]

  /**
   * A function that runs a `M` computation on the monadic state that was
   * captured by `liftBaseWith`.
   *
   * A `RunInBase` function yields a computation in the base monad of `M` that
   * returns the monadic state of `M`. This state can later be used to restore
   * the `M` computation using `restoreM`.
   */
  type RunInBase = Forall[({type l[a] = M[a] => B[StM[a]]})#l]

  /**
   * `liftBaseWith` is similar to `liftIO` in that it lifts a base computation
   * to the constructed monad.
   *
   * Instances should satisfy similar laws as the `MonadIO` laws:
   *
   *     liftBaseWith . const . return = return
   *     liftBaseWith (const (m >>= f)) = liftBaseWith (const m) >>= liftBaseWith . const . f
   *
   * The difference is that before lifting the base computation `liftBaseWith`
   * captures the state of `M`. It then provides the base computation with a
   * `RunInBase` function that allows running `M` computations in the base monad
   * on the captured state.
   */
  def liftBaseWith[A](f: RunInBase => B[A]): M[A]

  /**
   * Construct a `M` computation from the monadic state of `M` that is returned
   * from a `RunInBase` function.
   *
   * Instances should satisfy:
   *
   *    liftBaseWith (\runInBase -> runInBase m) >>= restoreM = m
   */
  def restoreM[A](st: StM[A]): M[A]

  /**
   * An often used composition: `control f = liftBaseWith f >>= restoreM`
   */
  final def control[A](f: RunInBase => B[StM[A]])(implicit M: Monad[M]): M[A] =
    M.bind(liftBaseWith[StM[A]](f))(restoreM[A](_))
}

object MonadBaseControl {
  @inline def apply[B[_], M[_]](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, M] = BM

  def identityMonadBaseControl[M[_]: Monad]: MonadBaseControl[M, M] =
    new MonadBaseControl[M, M] {
      type StM[A] = A

      def liftBaseWith[A](f: RunInBase => M[A]): M[A] =
        f(new RunInBase {
          def apply[X]: M[X] => M[X] = identity[M[X]](_)
        })

      def restoreM[A](st: StM[A]): M[A] =
        Monad[M].point(st)
    }

  def defaultMonadBaseControl[T[_[_], _], B[_], M[_]: Monad](implicit T: MonadTransControl[T[?[_], ?]], BM: MonadBaseControl[B, M]): MonadBaseControl[B, T[M, ?]] =
    new MonadBaseControl[B, T[M, ?]] {
      type StM[A] = BM.StM[T.StT[A]]

      def liftBaseWith[A](f: RunInBase => B[A]): T[M, A] =
        T.liftWith { run =>
          BM.liftBaseWith { runInBase =>
            f(new RunInBase {
              def apply[X]: T[M, X] => B[StM[X]] = { ta =>
                runInBase.apply[T.StT[X]](run(ta))
              }
            })
          }
        }

      def restoreM[A](st: StM[A]): T[M, A] =
        T.restoreT(BM.restoreM(st))
    }

  implicit val IOMonadBaseControl: MonadBaseControl[IO, IO] =
    identityMonadBaseControl[IO]

  implicit val OptionMonadBaseControl: MonadBaseControl[Option, Option] =
    identityMonadBaseControl[Option]

  implicit val MaybeMonadBaseControl: MonadBaseControl[Maybe, Maybe] =
    identityMonadBaseControl[Maybe]

  implicit val ListMonadBaseControl: MonadBaseControl[List, List] =
    identityMonadBaseControl[List]

  implicit val IListMonadBaseControl: MonadBaseControl[IList, IList] =
    identityMonadBaseControl[IList]

  implicit val StreamMonadBaseControl: MonadBaseControl[Stream, Stream] =
    identityMonadBaseControl[Stream]

  implicit def Function1MonadBaseControl[R]: MonadBaseControl[R => ?, R => ?] =
    identityMonadBaseControl[Function1[R, ?]]

  implicit def EitherMonadBaseControl[A]: MonadBaseControl[A \/ ?, A \/ ?] =
    identityMonadBaseControl[A \/ ?]

  implicit def IdTMonadBaseControl[B[_], M[_]: Monad](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, IdT[M, ?]] =
    defaultMonadBaseControl[IdT[?[_], ?], B, M]

  implicit def OptionTMonadBaseControl[B[_], M[_]: Monad](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, OptionT[M, ?]] =
    defaultMonadBaseControl[OptionT[?[_], ?], B, M]

  implicit def MaybeTMonadBaseControl[B[_], M[_]: Monad](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, MaybeT[M, ?]] =
    defaultMonadBaseControl[MaybeT[?[_], ?], B, M]

  implicit def ListTMonadBaseControl[B[_], M[_]: Monad](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, ListT[M, ?]] =
    defaultMonadBaseControl[ListT[?[_], ?], B, M]

  implicit def EitherTMonadBaseControl[B[_], M[_]: Monad, A](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, EitherT[M, A, ?]] =
    defaultMonadBaseControl[EitherT[?[_], A, ?], B, M]

  implicit def KleisliMonadBaseControl[B[_], M[_]: Monad, R](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, Kleisli[M, R, ?]] =
    defaultMonadBaseControl[Kleisli[?[_], R, ?], B, M]

  implicit def StateTMonadBaseControl[B[_], M[_]: Monad, S](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, StateT[M, S, ?]] =
    defaultMonadBaseControl[StateT[?[_], S, ?], B, M]

  implicit def WriterTMonadBaseControl[B[_], M[_]: Monad, W: Monoid](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, WriterT[M, W, ?]] =
    defaultMonadBaseControl[WriterT[?[_], W, ?], B, M]

  implicit def ReaderWriterStateTMonadBaseControl[B[_], M[_]: Monad, R, W: Monoid, S](implicit BM: MonadBaseControl[B, M]): MonadBaseControl[B, ReaderWriterStateT[M, R, W, S, ?]] =
    defaultMonadBaseControl[ReaderWriterStateT[?[_], R, W, S, ?], B, M]
}

