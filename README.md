Ported from the [monad-control](

This package defines the type class `MonadBaseControl`, a subset of `Monad` into which generic control operations such as `catch` can be lifted from `IO` or any other base monad. Instances are based on monad transformers in `MonadTransControl`, which includes all standard monad transformers.
