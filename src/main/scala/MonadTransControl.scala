package scalaz.monad.control

import scalaz._

trait MonadTransControl[T[_[_], _]] extends MonadTrans[T] {
  /** Monadic state of `T`. */
  type StT[A]

  /**
   * A function that runs a transformed monad `T` n on the monadic state that
   * was captured by `liftWith`
   *
   * A `Run` function yields a computation in `M` that returns the monadic state
   * of `T`. This state can later be used to restore a `T` computation using
   * `restoreT`.
   */
  abstract class Run {
    def apply[M[_]: Monad, A](ta: T[M, A]): M[StT[A]]
  }

  /**
   * `liftWith` is similar to `liftM` in that it lifts a computation from the
   * argument monad to the constructed monad.
   *
   * Instances should satisfy similar laws as the `MonadTrans` laws:
   *
   *     liftWith . const . return = return
   *     liftWith (const (m >>= f)) = liftWith (const m) >>= liftWith . const . f
   *
   * The difference with `liftM` is that before lifting the `M` computation
   * `liftWith` captures the state of `T`. It then provides the `M` computation
   * with a `Run` function that allows running `T[M, ?]` computations in `M`
   * (for all `M`) on the captured state.
   */
  def liftWith[M[_]: Monad, A](f: Run => M[A]): T[M, A]

  /**
   * Construct a `T` computation from the monadic state of `T` that is returned
   * from a `Run` function.
   *
   * Instances should satisfy:
   *
   *     liftWith (\run -> run t) >>= restoreT . return = t
   */
  def restoreT[M[_]: Monad, A](st: M[StT[A]]): T[M, A]

  /**
   * An often used composition: `control f = liftWith f >>= restoreT . return`
   */
  final def control[M[_]: Monad, A](f: Run => M[StT[A]]): T[M, A] =
    apply[M].bind(liftWith(f))(ta => restoreT(Monad[M].point(ta)))
}

object MonadTransControl {
  @inline def apply[F[_[_], _]](implicit F: MonadTransControl[F]): MonadTransControl[F] = F

  implicit val IdTMonadTransControl: MonadTransControl[IdT] =
    new MonadTransControl[IdT[?[_], ?]] {
      type StT[A] = A

      implicit def apply[G[_]: Monad]: Monad[IdT[G, ?]] = IdT.idTMonad[G]

      def liftM[G[_]: Monad, A](a: G[A]): IdT[G, A] =
        IdT.idTHoist.liftM(a)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): IdT[M, A] =
        IdT(f(new Run {
          def apply[N[_]: Monad, X](a: IdT[N, X]): N[X] = a.run
        }))

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): IdT[M, A] =
        IdT(st)
    }

  implicit def KleisliMonadTransControl[R]: MonadTransControl[Kleisli[?[_], R, ?]] =
    new MonadTransControl[Kleisli[?[_], R, ?]] {
      type StT[A] = A

      implicit def apply[G[_]: Monad]: Monad[Kleisli[G, R, ?]] =
        Kleisli.kleisliMonadTrans[R].apply[G]
      def liftM[G[_]: Monad, A](a: G[A]): Kleisli[G, R, A] =
        Kleisli.kleisliMonadTrans[R].liftM(a)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): Kleisli[M, R, A] =
        Kleisli(r => f(new Run {
          def apply[N[_]: Monad, X](ka: Kleisli[N, R, X]): N[X] = ka.run(r)
        }))

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): Kleisli[M, R, A] =
        Kleisli(_ => st)
    }

  implicit def StateTMonadTransControl[S]: MonadTransControl[StateT[?[_], S, ?]] =
    new MonadTransControl[StateT[?[_], S, ?]] {
      type StT[A] = (S, A)

      implicit def apply[G[_]: Monad]: Monad[StateT[G, S, ?]] =
        StateT.stateTMonadState[S, G]
      def liftM[G[_]: Monad, A](ga: G[A]): StateT[G, S, A] =
        StateT.StateMonadTrans[S].liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): StateT[M, S, A] =
        StateT { s =>
          val run = new Run {
            def apply[N[_]: Monad, X](sa: StateT[N, S, X]): N[StT[X]] = sa.run(s)
          }
          M.map(f(run))(a => (s, a))
        }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): StateT[M, S, A] =
        StateT(_ => st)
    }

  implicit val OptionTMonadTransControl: MonadTransControl[OptionT] =
    new MonadTransControl[OptionT] {
      type StT[A] = Option[A]

      implicit def apply[G[_]: Monad]: Monad[OptionT[G, ?]] =
        OptionT.optionTMonadTrans.apply[G]
      implicit def liftM[G[_]: Monad, A](ga: G[A]): OptionT[G, A] =
        OptionT.optionTMonadTrans.liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): OptionT[M, A] = {
        val run = new Run {
          def apply[N[_]: Monad, X](oa: OptionT[N, X]): N[StT[X]] = oa.run
        }
        OptionT(M.map(f(run))(Some(_)))
      }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): OptionT[M, A] = OptionT(st)
    }

  implicit val MaybeTMonadTransControl: MonadTransControl[MaybeT] =
    new MonadTransControl[MaybeT] {
      type StT[A] = Maybe[A]

      implicit def apply[G[_]: Monad]: Monad[MaybeT[G, ?]] =
        MaybeT.maybeTMonadTrans.apply[G]
      implicit def liftM[G[_]: Monad, A](ga: G[A]): MaybeT[G, A] =
        MaybeT.maybeTMonadTrans.liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): MaybeT[M, A] = {
        val run = new Run {
          def apply[N[_]: Monad, X](oa: MaybeT[N, X]): N[StT[X]] = oa.run
        }
        MaybeT(M.map(f(run))(Maybe.just(_)))
      }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): MaybeT[M, A] = MaybeT(st)
    }

  implicit def EitherTMonadTransControl[A]: MonadTransControl[EitherT[?[_], A, ?]] =
    new MonadTransControl[EitherT[?[_], A, ?]] {
      type StT[B] = A \/ B

      implicit def apply[G[_]: Monad]: Monad[EitherT[G, A, ?]] =
        EitherT.eitherTHoist[A].apply[G]
      implicit def liftM[G[_]: Monad, B](ga: G[B]): EitherT[G, A, B] =
        EitherT.eitherTHoist[A].liftM(ga)

      def liftWith[M[_], B](f: Run => M[B])(implicit M: Monad[M]): EitherT[M, A, B] = {
        val run = new Run {
          def apply[N[_]: Monad, X](ea: EitherT[N, A, X]): N[StT[X]] = ea.run
        }
        EitherT(M.map(f(run))(\/.right(_)))
      }

      def restoreT[M[_]: Monad, B](st: M[StT[B]]): EitherT[M, A, B] = EitherT(st)
    }

  implicit def LazyEitherTMonadTransControl[A]: MonadTransControl[LazyEitherT[?[_], A, ?]] =
    new MonadTransControl[LazyEitherT[?[_], A, ?]] {
      type StT[B] = LazyEither[A, B]

      implicit def apply[G[_]: Monad]: Monad[LazyEitherT[G, A, ?]] =
        LazyEitherT.lazyEitherTMonad[G, A]
      implicit def liftM[G[_], B](ga: G[B])(implicit M: Monad[G]): LazyEitherT[G, A, B] =
        LazyEitherT(M.map(ga)(LazyEither.lazyRight[A](_)))

      def liftWith[M[_], B](f: Run => M[B])(implicit M: Monad[M]): LazyEitherT[M, A, B] = {
        val run = new Run {
          def apply[N[_]: Monad, X](ea: LazyEitherT[N, A, X]): N[StT[X]] = ea.run
        }
        LazyEitherT(M.map(f(run))(LazyEither.lazyRight[A](_)))
      }

      def restoreT[M[_]: Monad, B](st: M[StT[B]]): LazyEitherT[M, A, B] = LazyEitherT(st)
    }

  implicit val LazyOptionTMonadTransControl: MonadTransControl[LazyOptionT] =
    new MonadTransControl[LazyOptionT] {
      type StT[A] = LazyOption[A]

      implicit def apply[G[_]: Monad]: Monad[LazyOptionT[G, ?]] =
        LazyOptionT.lazyOptionTMonadTrans.apply[G]
      implicit def liftM[G[_]: Monad, A](ga: G[A]): LazyOptionT[G, A] =
        LazyOptionT.lazyOptionTMonadTrans.liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): LazyOptionT[M, A] = {
        val run = new Run {
          def apply[N[_]: Monad, X](oa: LazyOptionT[N, X]): N[StT[X]] = oa.run
        }
        LazyOptionT(M.map(f(run))(LazyOption.lazySome(_)))
      }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): LazyOptionT[M, A] = LazyOptionT(st)
    }

  implicit val ListTMonadTransControl: MonadTransControl[ListT] =
    new MonadTransControl[ListT] {
      type StT[A] = List[A]

      implicit def apply[G[_]: Monad]: Monad[ListT[G, ?]] =
        ListT.listTHoist.apply[G]
      implicit def liftM[G[_]: Monad, A](ga: G[A]): ListT[G, A] =
        ListT.listTHoist.liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): ListT[M, A] = {
        val run = new Run {
          def apply[N[_]: Monad, X](oa: ListT[N, X]): N[StT[X]] = oa.run
        }
        ListT(M.map(f(run))(List(_)))
      }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): ListT[M, A] = ListT(st)
    }

  implicit def ReaderWriterStateTMonadTransControl[R, W: Monoid, S]: MonadTransControl[ReaderWriterStateT[?[_], R, W, S, ?]] =
    new MonadTransControl[ReaderWriterStateT[?[_], R, W, S, ?]] {
      type StT[A] = (W, A, S)

      implicit def apply[G[_]: Monad]: Monad[ReaderWriterStateT[G, R, W, S, ?]] =
        ReaderWriterStateT.rwstMonad[G, R, W, S]
      def liftM[G[_]: Monad, A](ga: G[A]): ReaderWriterStateT[G, R, W, S, A] =
        ReaderWriterStateT.rwstHoist[R, W, S].liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): ReaderWriterStateT[M, R, W, S, A] =
        ReaderWriterStateT { (r, s) =>
          val run = new Run {
            def apply[N[_]: Monad, X](sa: ReaderWriterStateT[N, R, W, S, X]): N[StT[X]] = sa.run(r, s)
          }
          M.map(f(run))(a => (Monoid[W].zero, a, s))
        }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): ReaderWriterStateT[M, R, W, S, A] =
        ReaderWriterStateT((_, _) => st)
    }

  implicit def WriterTMonadTransControl[W: Monoid]: MonadTransControl[WriterT[?[_], W, ?]] =
    new MonadTransControl[WriterT[?[_], W, ?]] {
      type StT[A] = (W, A)

      implicit def apply[G[_]: Monad]: Monad[WriterT[G, W, ?]] =
        WriterT.writerTMonad[G, W]
      def liftM[G[_]: Monad, A](ga: G[A]): WriterT[G, W, A] =
        WriterT.writerTHoist[W].liftM(ga)

      def liftWith[M[_], A](f: Run => M[A])(implicit M: Monad[M]): WriterT[M, W, A] =
        WriterT {
          val run = new Run {
            def apply[N[_]: Monad, X](sa: WriterT[N, W, X]): N[StT[X]] = sa.run
          }
          M.map(f(run))(a => (Monoid[W].zero, a))
        }

      def restoreT[M[_]: Monad, A](st: M[StT[A]]): WriterT[M, W, A] =
        WriterT(st)
    }
}

