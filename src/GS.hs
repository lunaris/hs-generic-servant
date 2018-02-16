{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module GS where

import qualified Control.Lens                               as Lens
import           Control.Monad.Except
import qualified Data.Aeson                                 as Ae
import qualified Data.Generics.Sum                          as G.S
import           Data.Proxy
import qualified Data.Vinyl                                 as Vl
import qualified Data.Vinyl.CoRec                           as Vl.Co
import qualified Data.Vinyl.Functor                         as Vl.F
import           GHC.Generics
import           GHC.TypeLits
import qualified Network.Wai                                as Wai
import qualified Network.Wai.Handler.Warp                   as Warp
import           Servant
import           Servant.Server.Internal.RoutingApplication

newtype Principal a
  = Principal { getPrincipal :: a }
  deriving (Eq, Ord, Show)

newtype Resource a
  = Resource { getResource :: a }
  deriving (Eq, Ord, Show)

newtype SessionId
  = SessionId { _sessionIdString :: String }
  deriving (Eq, Ord, Show)

newtype AccountId
  = AccountId { _accountIdString :: String }
  deriving (Eq, Ord, Show)

newtype Op1Result
  = Op1Result { _op1ResultString :: String }
  deriving stock   (Eq, Ord, Show)
  deriving newtype (Ae.ToJSON)

data Op1Error1
  = Op1Error1
  deriving stock    (Eq, Generic, Ord, Show)
  deriving anyclass (Ae.ToJSON)

data Op1Error2
  = Op1Error2
  deriving stock    (Eq, Generic, Ord, Show)
  deriving anyclass (Ae.ToJSON)

data Op2Result
  = Op2Result
      { _op2String :: String
      , _op2Int    :: Int
      }

  deriving stock    (Eq, Generic, Ord, Show)
  deriving anyclass (Ae.ToJSON)

data Op2Error1
  = Op2Error1
  deriving stock    (Eq, Generic, Ord, Show)
  deriving anyclass (Ae.ToJSON)

class Monad m => MonadService m where
  op1
    :: (G.S.AsType Op1Error1 e,
        G.S.AsType Op1Error2 e)

    => Principal SessionId
    -> Principal AccountId
    -> Resource AccountId
    -> ExceptT e m Op1Result

  op2
    :: G.S.AsType Op2Error1 e
    => Principal SessionId
    -> Principal AccountId
    -> Resource AccountId
    -> ExceptT e m Op2Result

op1Impl
  :: forall m e.
     (G.S.AsType Op1Error1 e,
      G.S.AsType Op1Error2 e,
      MonadIO m)

  => Principal SessionId
  -> Principal AccountId
  -> Resource AccountId
  -> ExceptT e m Op1Result

op1Impl prSessId prAccId rAccId = do
  logOperation "op1" prSessId prAccId rAccId

  case rAccId of
    Resource (AccountId "e1") -> throwError $ G.S.injectTyped Op1Error1
    Resource (AccountId "e2") -> throwError $ G.S.injectTyped Op1Error2
    _                         -> pure (Op1Result "op1")

op2Impl
  :: forall m e.
     (G.S.AsType Op2Error1 e,
      MonadIO m)

  => Principal SessionId
  -> Principal AccountId
  -> Resource AccountId
  -> ExceptT e m Op2Result

op2Impl prSessId prAccId rAccId = do
  logOperation "op2" prSessId prAccId rAccId

  case rAccId of
    Resource (AccountId "e1") -> throwError $ G.S.injectTyped Op2Error1
    _                         -> pure (Op2Result "op2" 42)

logOperation
  :: MonadIO m
  => String
  -> Principal SessionId
  -> Principal AccountId
  -> Resource AccountId
  -> m ()

logOperation op (Principal prSessId) (Principal prAccId) (Resource rAccId)
  = liftIO $ do
      putStrLn ""
      putStrLn $ "=============================================================="
      putStrLn $ "Operation:            " ++ op
      putStrLn $ "Principal session ID: " ++ _sessionIdString prSessId
      putStrLn $ "Principal account ID: " ++ _accountIdString prAccId
      putStrLn $ "Resource account ID:  " ++ _accountIdString rAccId
      putStrLn $ "=============================================================="
      putStrLn ""

newtype AnError es
  = AnError { _anErrorCoRec :: Vl.Co.CoRec Vl.F.Identity es }

instance {-# OVERLAPPING #-}
         (Vl.RElem e es i, Vl.RecApplicative es)
      =>  G.S.AsType e (AnError es) where

  _Typed
    = Lens.prism' G.S.injectTyped G.S.projectTyped

  injectTyped
    = AnError . Vl.Co.CoRec . Vl.F.Identity

  projectTyped
    = Vl.Co.asA (Proxy @e) . _anErrorCoRec

type API
  =       "op1"
       :> Errors
           '[ Op1Error1 `AsStatus` 409
            , Op1Error2 `AsStatus` 418
            ]

       :> Get '[JSON] Op1Result

  :<|>    "op2"
       :> Get '[JSON] Op2Result

data Errors es

instance (HandledErrors es, HasServer api ctx)
      =>  HasServer ((:>) (Errors es) api) ctx where

  type ServerT ((:>) (Errors es) api) m
    = ErrorHandlersFor es m -> ServerT api m

  route Proxy context subserver
    = route (Proxy @api) context
        (passToServer subserver (const (errorHandlersFor @es)))

type AsStatus e sts
  = '(e, sts)

newtype ErrorHandlersFor es m
  = ErrorHandlersFor (forall a. AnError (Fsts es) -> m a)

errorHandlersFor
  :: forall es m.
     (HandledErrors es, MonadError ServantErr m)
  => ErrorHandlersFor es m

errorHandlersFor
  = ErrorHandlersFor $ \(AnError x) ->
      Vl.Co.match @(Fsts es) x (errorHandlersFor' @es)

class HandledErrors (es :: [(*, Nat)]) where
  errorHandlersFor'
    :: MonadError ServantErr m
    => Vl.Co.Handlers (Fsts es) (m a)

instance HandledErrors '[] where
  errorHandlersFor'
    = Vl.RNil

instance (Ae.ToJSON e, ErrorStatusCode sts, HandledErrors es)
      =>  HandledErrors ( '(e, sts) ': es) where

  errorHandlersFor'
    =     (Vl.Co.H $ \e -> throwError $
            (statusCodeError @sts) { errBody = Ae.encode e })

    Vl.:& errorHandlersFor' @es

class KnownNat sts => ErrorStatusCode sts where
  statusCodeError :: ServantErr

instance ErrorStatusCode 409 where
  statusCodeError
    = err409

instance ErrorStatusCode 418 where
  statusCodeError
    = err418

type family Fsts (xs :: [(a, b)]) :: [a] where
  Fsts '[]              = '[]
  Fsts ( '(a, b) ': xs) = a ': Fsts xs

server :: Server API
server
  =    ( \(ErrorHandlersFor handleErrors) ->
            runExceptT (op1Impl prSessId prAccId (Resource $ AccountId "e2"))
              >>= either handleErrors pure

       )

  :<|> pure (Op2Result "op2" 42)

  where
    prSessId = Principal $ SessionId "prSessId"
    prAccId  = Principal $ AccountId "prAccId"

app :: Wai.Application
app
  = serve (Proxy @API) server

main :: IO ()
main
  = Warp.run 9393 app
