
module ApplicationHandlers where
-- FIXME: reexport AppHandlers/* & remove import AppHandlers.* from AppInit

import Prelude hiding (log)

import Data.Functor
import Control.Monad

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String (fromString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Time

import Data.Aeson (object, (.=))

import System.Locale

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.SimpleLog
import Snap.Snaplet.Vin
import Snap.Util.FileServe (serveFile)
------------------------------------------------------------------------------
import qualified Snaplet.DbLayer as DB
import qualified Snaplet.DbLayer.RKC as RKC
import Snaplet.FileUpload (doUpload', doDeleteAll')
------------------------------------------------------------------------------
import qualified Nominatim
-----------------------------------------------------------------------------
import Application
import AppHandlers.MyActions
import AppHandlers.Util
import Util


------------------------------------------------------------------------------
-- | Render empty form for model.
indexPage :: AppHandler ()
indexPage = ifTop $ render "index"


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
  serveFile $ "snaplets/heist/resources/templates/login.html"


------------------------------------------------------------------------------
-- | Login user.
doLogin :: AppHandler ()
doLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- isJust <$> getParam "remember"
  res <- with auth $ loginByUsername l (ClearText p) r
  case res of
    Left _ -> redirectToLogin
    Right u -> do
      creds <- (,)
        <$> getParam "avayaExt"
        <*> getParam "avayaPwd"
      case creds of
        (Just ext, Just pwd) -> with auth $ saveUser
          u {userMeta
            = HashMap.insert "avayaExt" (Aeson.toJSON ext)
            $ HashMap.insert "avayaPwd" (Aeson.toJSON pwd)
            $ userMeta u
            }
      addToLoggedUsers u

      redirect "/"


doLogout :: AppHandler ()
doLogout = ifTop $ do
  Just u <- with auth currentUser
  rmFromLoggedUsers u
  with auth logout
  redirectToLogin

------------------------------------------------------------------------------
-- | Serve user account data back to client.
serveUserCake :: AppHandler ()
serveUserCake = ifTop
  $ with auth currentUser
  >>= maybe (error "impossible happened") writeJSON


------------------------------------------------------------------------------
-- | Geodecode mockup.
smspost :: AppHandler ()
smspost = do
  Just smsId <- getParam "smsId"
  Right _ <- with db $ DB.submitTask "smspost" smsId
  writeBS ""


------------------------------------------------------------------------------
-- | Geodecode mockup.
geodecode :: AppHandler ()
geodecode = ifTop $ do
  addr <- fromMaybe "Moscow" <$> getParam "addr"
  resp <- liftIO $ Nominatim.geodecode addr
  writeJSON resp

------------------------------------------------------------------------------
-- | CRUD
createHandler :: AppHandler ()
createHandler = do
  Just model <- getParam "model"
  commit <- getJSONBody
  res <- with db $ DB.create model commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

readHandler :: AppHandler ()
readHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res <- with db $ DB.read model objId
  -- FIXME: try/catch & handle/log error
  writeJSON res

readAllHandler :: AppHandler ()
readAllHandler = do
  Just model <- getParam "model"
  (with db $ DB.readAll model)
    >>= apply "orderby" sortBy (flip . comparing . Map.lookup)
    >>= apply "limit"   take   (read . B.unpack)
    >>= apply "select"  filter flt
    >>= apply "fields"  map    proj
    >>= writeJSON
  where
    apply name f g = \xs
      -> maybe xs (\p -> f (g p) xs)
      <$> getParam name

    proj fs = \obj -> Map.fromList
      [(k, Map.findWithDefault "" k obj)
      | k <- B.split ',' fs
      ]

    flt prm = \obj -> all (selectParse obj) $ B.split ',' prm

updateHandler :: AppHandler ()
updateHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  commit <- getJSONBody
  -- Need this hack, or server won't return updated "cost_counted"
  res <- with db $ DB.update model objId $ Map.delete "cost_counted" commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

deleteHandler :: AppHandler ()
deleteHandler = do
  Just model <- getParam "model"
  Just objId <- getParam "id"
  res        <- with db $ DB.delete model objId
  writeJSON res

syncHandler :: AppHandler ()
syncHandler = scope "sync" $ do
  mdl <- getParam "model"
  from <- liftM (fmap (maybe 0 fst . B.readInt)) $ getParam "from"
  log Info $ T.concat ["Syncing ", maybe "all" T.decodeUtf8 mdl, " model(s) starting from id ", maybe "1" (fromString . show) from]
  res <- with db $ DB.sync mdl from
  writeJSON res

searchHandler :: AppHandler ()
searchHandler = scope "searchHandler" $ do
  Just q <- getParam "q"
  Just m <- getParam "model"
  Just fs <- getParam "fields"
  Just sel <- getParam "select"
  let
    getInt v = do
      s <- v
      (x, _) <- B.readInt s
      return x
    sels = B.split ',' sel
  lim <- liftM (maybe 100 id . getInt) $ getParam "limit"
  res <- with db $ DB.searchFullText m (B.split ',' fs) sels q lim
  writeJSON $ map (Map.fromList . zip sels) res

rkcHandler :: AppHandler ()
rkcHandler = scope "rkcHandler" $ do
  p <- getParam "program"
  c <- getParam "city"
  info <- with db $ RKC.rkc (maybe T.empty T.decodeUtf8 p) (maybe T.empty T.decodeUtf8 c)
  writeJSON info



searchCallsByPhone :: AppHandler ()
searchCallsByPhone = do
  r <- getRequest
  calls <- with db $ DB.readAll "call"
  let phone = last $ B.split '/' (rqURI r)
  writeJSON $
    filter ((phone ==) . (Map.findWithDefault "" "callerName_phone1")) calls

getActionsForCase :: AppHandler ()
getActionsForCase = do
  Just id <- getParam "id"
  actions <- with db $ DB.readAll "action"
  let id' = B.append "case:" id
  writeJSON $
    filter ((id' ==) . (Map.findWithDefault "" "caseId")) actions

-- | This action recieve model and id as parameters to lookup for
-- and json object with values to create new model with specified
-- id when it's not found
findOrCreateHandler :: AppHandler ()
findOrCreateHandler = do
  Just model <- getParam "model"
  Just id    <- getParam "id"
  commit <- getJSONBody
  res <- with db $ DB.findOrCreate model id commit
  -- FIXME: try/catch & handle/log error
  writeJSON res

------------------------------------------------------------------------------
-- | Reports
report :: AppHandler ()
report = scope "report" $ do
  Just reportId <- getParam "program"
  fromDate <- liftM (fmap T.decodeUtf8) $ getParam "from"
  toDate <- liftM (fmap T.decodeUtf8) $ getParam "to"
  reportInfo <- with db $ DB.read "report" reportId
  tz <- liftIO getCurrentTimeZone
  let tplName = B.unpack (reportInfo Map.! "templates")
  log Info $ T.concat ["Generating report ", T.pack tplName]
  let template
        = "resources/static/fileupload/report/"
        ++ (B.unpack reportId) ++ "/templates/" ++ tplName
  let result = "resources/reports/" ++ tplName
  let
    -- convert format and UTCize time, and apply f to UTCTime
    validateAnd f dateStr = fmap (format . f . toUTC) $ parse dateStr where
      format = T.pack . formatTime defaultTimeLocale "%d.%m.%Y %X"
      parse :: T.Text -> Maybe LocalTime
      parse = parseTime defaultTimeLocale "%d.%m.%Y" . T.unpack
      toUTC = localTimeToUTC tz
    withinAnd f pre post dateValue = do
      v <- dateValue
      s <- validateAnd f v
      return $ T.concat [T.pack pre, s, T.pack post]

    fromTo mdl = (from, to) where
      from = withinAnd id (mdl ++ " >= to_timestamp('") "', 'DD.MM.YYYY HH24:MI:SS')" fromDate
      to = withinAnd addDay (mdl ++ " < to_timestamp('") "', 'DD.MM.YYYY HH24:MI:SS')" toDate

    addDay tm = tm { utctDay = addDays 1 (utctDay tm) }
    
    mkCondition nm = catMaybes [from', to'] where
      (from', to') = fromTo (T.unpack nm)
  
  with db $ DB.generateReport mkCondition template result
  modifyResponse $ addHeader "Content-Disposition" "attachment; filename=\"report.xlsx\""
  serveFile result

createReportHandler :: AppHandler ()
createReportHandler = do
  res <- with db $ DB.create "report" $ Map.empty
  let id = last $ B.split ':' $ fromJust $ Map.lookup "id" res
  (f:_)      <- with fileUpload $ doUpload' "report" id "templates"
  Just name  <- getParam "name"
  -- we have to update all model params after fileupload,
  -- because in multipart/form-data requests we do not have
  -- params as usual, see Snap.Util.FileUploads.setProcessFormInputs
  with db $ DB.update "report" id $
    Map.fromList [ ("templates", BU.fromString f)
                 , ("name",      name) ]
  redirect "/#reports"

deleteReportHandler :: AppHandler ()
deleteReportHandler = do
  Just id  <- getParam "id"
  with db $ DB.delete "report" id
  with fileUpload $ doDeleteAll' "report" id
  return ()

getUsersDict :: AppHandler ()
getUsersDict = writeJSON =<< gets allUsers

------------------------------------------------------------------------------
-- | Utility functions
vinUploadData :: AppHandler ()
vinUploadData = scope "vin" $ scope "upload" $ do
  log Trace "Uploading data"
  (f:_) <- with fileUpload $ doUpload' "report" "upload" "data"
  log Trace $ T.concat ["Uploaded to file: ", T.pack f]
  prog <- getParam "program"
  case prog of
    Nothing -> log Error "Program not specified"
    Just p -> do
      log Info $ T.concat ["Uploading ", T.pack f]
      log Trace $ T.concat ["Program: ", T.decodeUtf8 p]
      log Trace $ T.concat ["Initializing state for file: ", T.pack f]
      with vin $ initUploadState f
      log Trace $ T.concat ["Uploading data from file: ", T.pack f]
      with vin $ uploadData (T.unpack . T.decodeUtf8 $ p) f

vinStateRead :: AppHandler ()
vinStateRead = scope "vin" $ scope "state" $ scope "get" $ do
  log Trace "Getting state"
  with vin getState

vinStateRemove :: AppHandler ()
vinStateRemove = scope "vin" $ scope "state" $ scope "remove" $ do
  log Trace "Remove alert by id"
  res <- getParam "id"
  log Trace $ T.concat ["id: ", maybe "<null>" (T.pack . show) res]
  with vin removeAlert

getSrvTarifOptions :: AppHandler ()
getSrvTarifOptions = do
  Just id    <- getParam "id"
  Just model <- getParam "model"
  srv     <- with db $ DB.read model id
  partner <- with db $ get $ B.split ':' $
             fromMaybe "" $ Map.lookup "contractor_partnerId" srv
  -- partner services with same serviceName as current service model
  partnerSrvs <- with db $ mapM get $ getIds "services" partner
  case filter (mSrv model) partnerSrvs of
    []     -> return ()
    (x:xs) -> do
      tarifOptions <- with db $ mapM get $ getIds "tarifOptions" x
      writeJSON $ map rebuilOpt tarifOptions
  where
      getIds f m = map (B.split ':') $ B.split ',' $
                   fromMaybe "" $ Map.lookup f m
      get [m, id] = Map.insert "id" id <$> DB.read m id
      get _       = return $ Map.empty
      mSrv m = (m ==) . fromMaybe "" . Map.lookup "serviceName"
      rebuilOpt :: Map ByteString ByteString -> Map ByteString ByteString
      rebuilOpt o = Map.fromList $
                    [("id"        , fromMaybe "" $ Map.lookup "id" o)
                    ,("optionName", fromMaybe "" $ Map.lookup "optionName" o)]

smsProcessingHandler :: AppHandler ()
smsProcessingHandler = scope "sms" $ do
  res <- with db DB.smsProcessing
  writeJSON $ object [
    "processing" .= res]

errorsHandler :: AppHandler ()
errorsHandler = do
  l <- gets feLog
  r <- readRequestBody 4096
  liftIO $ withLog l $ scope "frontend" $ do
  log Info $ toStrict $ decodeUtf8 r

createCaseByPhone :: AppHandler ()
createCaseByPhone = do
  phone <- last . B.split '/' . rqURI <$> getRequest
  findCasesByPhone phone >>= writeJSON

findCasesByPhone phone = do
  cases <- with db $ DB.readAll "case"
  return $ sortBy (\a b -> ctime b `compare` ctime a)
    $ filter (\p -> Map.lookup "contact_phone1" p == Just phone) cases
  where ctime = Map.lookup "ctime"

copyCaseHandler :: AppHandler ()
copyCaseHandler = do
  Just id <- getParam "id"
  copyCase id >>= writeJSON

copyCase :: ByteString -> AppHandler (Map ByteString ByteString)
copyCase id = do
  let copyFields =
        [ "car_vin"
        , "car_seller"
        , "car_make"
        , "car_model"
        , "car_plateNum"
        , "car_makeYear"
        , "car_color"
        , "car_buyDate"
        , "car_transmission"
        , "car_engine"
        , "car_liters"
        , "car_capacity"
        , "car_dims"
        , "car_weight"
        , "car_checkPeriod"
        , "car_class"
        , "contact_name"
        , "contact_phone1"
        , "contact_phone2"
        , "contact_phone3"
        , "contact_phone4"
        , "contact_email"
        , "contact_contactOwner"
        , "contact_ownerName"
        , "contact_ownerEmail"
        , "contact_ownerPhone1"
        , "contact_ownerPhone2"
        , "contact_ownerPhone3"
        , "contact_ownerPhone4"
        ]
  kase <- with db $ DB.read "case" id
  let newCase = Map.fromList $ map (mkField kase) copyFields
  with db $ DB.create "case" newCase
    where
      mkField m f = (f, fromMaybe "" $ Map.lookup f m)