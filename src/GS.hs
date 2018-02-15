{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module GS where

import qualified Control.Lens             as Lens
import           Control.Monad.Except
import qualified Data.Aeson               as Ae
import qualified Data.Generics.Sum        as G.S
import           Data.Proxy
import qualified Data.Vinyl               as Vl
import qualified Data.Vinyl.CoRec         as Vl.Co
import qualified Data.Vinyl.Functor       as Vl.F
import           GHC.Generics
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

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
  deriving (Eq, Generic, Ord, Show)

data Op1Error2
  = Op1Error2
  deriving (Eq, Generic, Ord, Show)

data Op2Result
  = Op2Result
      { _op2String :: String
      , _op2Int    :: Int
      }

  deriving stock    (Eq, Generic, Ord, Show)
  deriving anyclass (Ae.ToJSON)

data Op2Error1
  = Op2Error1
  deriving (Eq, Generic, Ord, Show)

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
  = APIAround MonadService
      (    Invoke "op1" ("op1" :> Get '[JSON] Op1Result)
      :<|> Invoke "op2" ("op2" :> Get '[JSON] Op2Result)
      )

data APIAround c api

instance HasServer api ctx => HasServer (APIAround c api) ctx where
  type ServerT (APIAround c api) m
    = ServerT api m

  route _
    = route (Proxy @api)

data Invoke f api

instance HasServer api ctx => HasServer (Invoke f api) ctx where
  type ServerT (Invoke f api) m
    = ServerT api m

  route _
    = route (Proxy @api)

class GServer api where
  gserver :: Server api

instance (GServer a, GServer b) => GServer (a :<|> b) where
  gserver
    = gserver @a :<|> gserver @b

server' :: Server API
server'
  = gserver @API

server :: Server API
server
  =    pure (Op1Result "op1")
  :<|> pure (Op2Result "op2" 42)

app :: Wai.Application
app
  = serve (Proxy @API) server

main :: IO ()
main
  = Warp.run 9393 app
