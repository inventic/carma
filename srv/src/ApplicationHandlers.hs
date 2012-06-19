
module ApplicationHandlers where


import Data.Functor
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Snap.Core
import Snap.Snaplet (with)
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Session
import Snap.Util.FileServe (serveFile)
------------------------------------------------------------------------------
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Templates (index)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import Snaplet.SiteConfig
import Snap.Snaplet.AvayaAES
------------------------------------------------------------------------------
import qualified Codec.Xlsx.Templater as Xlsx
import qualified Nominatim
------------------------------------------------------------------------------
import Application


------------------------------------------------------------------------------
-- | Render empty form for model.
indexPage :: AppHandler ()
indexPage = do
  chk <- with auth isLoggedIn
  ifTop $ writeLBS $ renderHtml $ index chk ()


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "/login/" 303


------------------------------------------------------------------------------
-- | If user is not logged in, redirect to login page, pass to
-- handler otherwise.
authOrLogin :: AppHandler () -> AppHandler ()
authOrLogin h = requireUser auth redirectToLogin h


------------------------------------------------------------------------------
-- | Render empty login form.
loginForm :: AppHandler ()
loginForm = do
  serveFile $ "resources/templates/login.html"


------------------------------------------------------------------------------
-- | Login user.
doLogin :: AppHandler ()
doLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- maybe False (const True) <$> getParam "remember"
  res <- with auth $ loginByUsername l (ClearText p) r
  case res of
    Left _err -> redirectToLogin
    Right _user -> redirect "/"


------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AuthUser -> AppHandler ()
serveUserCake user = ifTop $ writeJSON user


------------------------------------------------------------------------------
-- | Geodecode mockup.
geodecode :: AppHandler ()
geodecode = ifTop $ do
  addr <- fromMaybe "Moscow" <$> getParam "addr"
  resp <- liftIO $ Nominatim.geodecode addr
  writeJSON resp


------------------------------------------------------------------------------
-- | CRUD
createHandler :: AuthUser -> AppHandler ()
createHandler curUser = do
  Just model <- getParam "model"
  Just commit <- Aeson.decode <$> getRequestBody
  res <- with db $ DB.create model commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

readHandler :: AuthUser -> AppHandler ()
readHandler curUser = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res <- with db $ DB.read model objId
  -- FIXME: try/catch & handle/log error
  writeJSON res

readAllHandler :: AuthUser -> AppHandler ()
readAllHandler curUser = do
  Just model <- getParam "model"
  res <- with db $ DB.readAll model
  writeJSON res

updateHandler :: AuthUser -> AppHandler ()
updateHandler curUser = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  Just commit <- Aeson.decode <$> getRequestBody
  res <- with db $ DB.update model objId commit
  -- FIXME: try/catch & handle/log error
  writeJSON res


------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = do
  liftIO $ Xlsx.run
    "resources/report-templates/all-cases.xlsx"
    "resources/static/all-cases.xlsx"
    [(Map.empty, Xlsx.TemplateSettings Xlsx.Rows 1, [])]
  serveFile "resources/static/all-cases.xlsx"


------------------------------------------------------------------------------
-- | Utility functions
writeJSON :: Aeson.ToJSON v => v -> AppHandler ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ Aeson.encode v
