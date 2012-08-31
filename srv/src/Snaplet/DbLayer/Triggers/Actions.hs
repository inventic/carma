module Snaplet.DbLayer.Triggers.Actions where

import Control.Arrow (first)
import Control.Monad (when,void)
import Control.Monad.Trans
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.Map as Map
import Data.Char
import Data.Maybe

import qualified Fdds as Fdds
------------------------------------------------------------------------------
import WeatherApi (getWeather', tempC)
import WeatherApi.Google (initApi)
-----------------------------------------------------------------------------
import Data.Time.Format (parseTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)

import Snap (gets)
import Snap.Snaplet.RedisDB
import qualified Database.Redis as Redis
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl

import Util

services =
  ["deliverCar"
  ,"deliverParts"
  ,"hotel"
  ,"information"
  ,"rent"
  ,"sober"
  ,"taxi"
  ,"tech"
  ,"towage"
  ,"transportation"
  ]

actions :: TriggerMap a
actions = Map.fromList
  $ [(s,serviceActions) | s <- services]
  ++[("action", actionActions)
    ,("case", Map.fromList
      [("city", [\objId val -> do
                  oldCity <- lift $ runRedisDB redis $ Redis.hget objId "city"
                  case oldCity of
                    Left _         -> return ()
                    Right Nothing  -> setWeather objId val
                    Right (Just c) -> when (c /= val) $ setWeather objId val
                  ])
      ,("car_vin", [\objId val ->
        if B.length val /= 17
          then return ()
          else do
            let vinKey = B.concat ["vin:", B.map toUpper val]
            car <- lift $ runRedisDB redis
                        $ Redis.hgetall vinKey
            case car of
              Left _    -> return ()
              Right []  -> do
                res <- requestFddsVin objId val
                set objId "vinChecked"
                  $ if res then "fdds" else "vinNotFound"
              Right car -> do
                set objId "vinChecked" "base"
                mapM_ (uncurry $ set objId)
                  $ map (first $ B.append "car_") car
      ]
      )]
    )]

-- Создания действий "с нуля"
serviceActions = Map.fromList
  [("status", [\objId val ->
    case val of
      "backoffice" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "orderService")
            ,("duetime", due)
            ,("description", utf8 "Заказать услугу")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "mechanicConf" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "mechanicConf")
            ,("duetime", due)
            ,("description", utf8 "Требуется конференция с механиком")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "dealerConf" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerConf")
            ,("duetime", due)
            ,("description", utf8 "Требуется конференция с дилером")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "dealerConformation" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "dealerApproval")
            ,("duetime", due)
            ,("description", utf8 "Требуется согласование с дилером")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "makerConformation" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "carmakerApproval")
            ,("duetime", due)
            ,("description", utf8 "Требуется согласование с заказчиком программы")
            ,("targetGroup", "back")
            ,("priority", "2")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId
      "clientCanceled" -> do
          due <- dateNow (+ (1*60))
          kazeId <- get objId "parentId"
          actionId <- new "action" $ Map.fromList
            [("name", "cancelService")
            ,("duetime", due)
            ,("description", utf8 "Клиент отказался от услуги (сообщил об это оператору Front Office)")
            ,("targetGroup", "back")
            ,("priority", "1")
            ,("parentId", objId)
            ,("caseId", kazeId)
            ,("closed", "0")
            ]
          upd kazeId "actions" $ addToList actionId             
      _ -> return ()]
  )
  ,("contractor_partner",
    [\objId val -> do
        opts <- get objId "service_tarifOptions"
        let ids = B.split ',' opts
        lift $ runRedisDB redis $ Redis.del ids
        set objId "service_tarifOptions" ""
    ])
  -- ,("service_tarifOptions",
  --   [\objId val -> do
  --     let ids = B.split ',' val
  --     opts <- lift $ runRedisDB redis $ mapM Redis.hgetall ids
  --     return ()
  --     ])
  ]

resultSet1 =
  ["partnerNotOk", "caseOver", "partnerFound"
  ,"carmakerApproved", "dealerApproved", "needService"
  ] 

actionActions = Map.fromList
  [("closed",
    [\objId val -> do
        comment <- get objId "comment"
        when (val == "1") $
          set objId "comment" $
          B.append comment $ utf8 "\nЗакрыто супервизором"
    ])
  ,("assignedTo",
    [\objId val -> do
        comment <- get objId "comment"
        Right oldVal <- lift $ runRedisDB redis $ Redis.hget objId "assignedTo"
        UsersDict allu <- lift $ gets allUsers
        when (any (== val) $ map (fromJust . (Map.lookup "value")) allu) $
          case oldVal of
            Just v  -> set objId "comment" $ B.append comment $ B.concat
                       [ utf8 "\nОтвественный изменен супервизором c "
                       , v , utf8 " на " , val
                       ]
            Nothing  -> set objId "comment" $ B.append comment $ B.concat
                       [ utf8 "\nОтвественный изменен супервизором на "
                       , val
                       ]
    ])
  ,("result",
    [\objId val -> when (val `elem` resultSet1) $ do
         setService objId "status" "orderService"
         void $ replaceAction
             "orderService"
             "Заказать услугу"
             "back" "1" (+5*60) objId
    ,\objId val -> maybe (return ()) ($objId)
      $ Map.lookup val actionResultMap
    ]
  )]

actionResultMap = Map.fromList
  [("busyLine",        \objId -> dateNow (+ (5*60))  >>= set objId "duetime" >> set objId "result" "")
  ,("callLater",       \objId -> dateNow (+ (30*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("bigDelay",        \objId -> dateNow (+ (6*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("partnerNotFound", \objId -> dateNow (+ (2*60*60)) >>= set objId "duetime" >> set objId "result" "")
  ,("clientCanceledService", closeAction)   
  ,("unassignPlease",  \objId -> set objId "assignedTo" "" >> set objId "result" "")
  ,("needPartner",     \objId -> do 
     setService objId "status" "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         "parguy" "1" (+60) objId
     set newAction "assignedTo" ""
  )
  ,("serviceOrdered", \objId -> do
     setService objId "status" "serviceOrdered"
     void $ replaceAction
         "tellClient"
         "Сообщить клиенту о договорённости" 
         "back" "1" (+60) objId
  )
  ,("partnerNotOk", void . replaceAction
      "cancelService"
      "Требуется отказаться от заказанной услуги"
      "back" "1" (+60)
  )
  ,("moveToAnalyst", \objId -> do
    act <- replaceAction
      "orderServiceAnalyst"
      "Заказ услуги аналитиком"
      "analyst" "1" (+60) objId
    set act "assignedTo" ""
  )
  ,("moveToBack", \objId -> do
    act <- replaceAction
      "orderService"
      "Заказ услуги аналитиком"
      "back" "1" (+60) objId
    set act "assignedTo" ""
  )
  ,("needPartnerAnalyst",     \objId -> do 
     setService objId "status" "needPartner"
     newAction <- replaceAction
         "needPartner"
         "Требуется найти партнёра для оказания услуги"
         "parguy" "1" (+60) objId
     set newAction "assignedTo" ""
  )  
  ,("serviceOrderedAnalyst", \objId -> do
     setService objId "status" "serviceOrdered"
     void $ replaceAction
         "tellClient"
         "Сообщить клиенту о договорённости" 
         "back" "1" (+60) objId
  )  
  ,("partnerNotOkCancel", \objId -> do
      setService objId "status" "cancelService"
      void $ replaceAction
         "cancelService"
         "Требуется отказаться от заказанной услуги"
         "back" "1" (+60) objId
  )
  ,("partnerOk", \objId -> do
    tm <- getService objId "times_expectedServiceStart"
    void $ replaceAction
      "checkStatus"
      "Уточнить статус оказания услуги"
      "back" "3" (changeTime (+5*60) tm)
      objId
  )
  ,("serviceDelayed", \objId -> do
    setService objId "status" "serviceDelayed"
    void $ replaceAction
      "tellDelayClient"
      "Сообщить клиенту о задержке начала оказания услуги"
      "back" "1" (+60)
      objId
  )
  ,("serviceInProgress", \objId -> do
    setService objId "status" "serviceInProgress"
    tm <- getService objId "times_expectedServiceEnd"
    void $ replaceAction
      "checkEndOfService"
      "Уточнить у клиента окончено ли оказание услуги"
      "back" "3" (changeTime (+5*60) tm)
      objId
  )  
  ,("serviceStillInProgress", \objId -> do
    tm <- getService objId "times_expectedServiceEnd"  
    dateNow (changeTime (+5*60) tm) >>= set objId "duetime"
    set objId "result" "") 
  ,("clientWaiting", \objId -> do
    tm <- getService objId "times_expectedServiceStart"
    void $ replaceAction
      "checkStatus"
      "Уточнить статус оказания услуги"
      "back" "3" (changeTime (+5*60) tm)
      objId
  )
  ,("serviceFinished", \objId -> do
    setService objId "status" "serviceOk"
    tm <- getService objId "times_expectedServiceClosure"  
    void $ replaceAction
      "closeCase"
      "Закрыть заявку"
      "back" "3" (changeTime (+5*60) tm)
      objId
    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+14*24*60*60)
      objId
    set act "assignedTo" ""
    void $ replaceAction
      "getInfoDealerVW"
      "Требуется уточнить информацию о ремонте у дилера (только для VW)"
      "analyst" "3" (+7*24*60*60)
      objId
  )
  ,("complaint", \objId -> do
    setService objId "status" "serviceOk"
    setService objId "clientSatisfied" "0"
    tm <- getService objId "times_expectedServiceClosure"    
    act1 <- replaceAction
      "complaintResolution"
      "Клиент предъявил претензию"
      "supervisor" "1" (+60)
      objId 
    set act1 "assignedTo" ""
    void $ replaceAction
      "closeCase"
      "Закрыть заявку"
      "back" "3" (changeTime (+5*60) tm)
      objId
    act2 <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+14*24*60*60)
      objId
    set act2 "assignedTo" ""
    void $ replaceAction
      "getInfoDealerVW"
      "Требуется уточнить информацию о ремонте у дилера (только для VW)"
      "back" "3" (+7*24*60*60)
      objId
  )
  ,("billNotReady", \objId -> dateNow (+ (5*24*60*60))  >>= set objId "duetime")
  ,("billAttached", \objId -> do
    act <- replaceAction
      "headCheck"
      "Проверка РКЦ"
      "head" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("parguyToBack", \objId -> do
    act <- replaceAction
      "parguyNeedInfo"
      "Менеджер по Партнёрам запросил доп. информацию"
      "back" "3" (+360) objId
    set act "assignedTo" ""
  )
  ,("backToParyguy", \objId -> do
    act <- replaceAction
      "addBill"
      "Прикрепить счёт"
      "parguy" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("headToParyguy", \objId -> do
    act <- replaceAction
      "addBill"
      "На доработку МпП"
      "parguy" "1" (+360) objId
    set act "assignedTo" ""
  ) 
  ,("confirm", \objId -> do
    act <- replaceAction
      "directorCheck"
      "Проверка директором"
      "director" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("confirmWODirector", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверка бухгалтерией"
      "account" "1" (+360) objId
    set act "assignedTo" ""
  )  
  ,("confirmFinal", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      "analyst" "1" (+360) objId
    set act "assignedTo" ""
  )    
  ,("directorToHead", \objId -> do
    act <- replaceAction
      "headCheck"
      "Проверка РКЦ"
      "head" "1" (+360) objId
    set act "assignedTo" ""
  )
  ,("directorConfirm", \objId -> do
    act <- replaceAction
      "accountCheck"
      "Проверка бухгалтерией"
      "account" "1" (+360) objId
    set act "assignedTo" ""
  )      
  ,("dirConfirmFinal", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      "analyst" "1" (+360) objId
    set act "assignedTo" ""
  )    
  ,("vwclosed", closeAction
  )   
  ,("accountConfirm", \objId -> do
    act <- replaceAction
      "analystCheck"
      "Обработка аналитиком"
      "analyst" "1" (+360) objId
    set act "assignedTo" ""
  )   
  ,("accountToDirector", \objId -> do
    act <- replaceAction
      "directorCheck"
      "Проверка директором"
      "director" "1" (+360) objId
    set act "assignedTo" ""
  )   
  ,("analystChecked", closeAction
  )    
  ,("caseClosed", \objId -> do
    setService objId "status" "serviceClosed"
    closeAction objId	
  )
  ,("falseCallWBill", \objId -> do
     setService objId "falseCall" "bill"
     closeAction objId
  )
  ,("falseCallWOBill", \objId -> do
     setService objId "falseCall" "nobill"
     closeAction objId
  )
  ,("clientNotified", \objId -> do
     setService objId "status" "serviceClosed"
     closeAction objId
  ) 
  ,("notNeedService", \objId -> do
     setService objId "status" "serviceClosed"
     closeAction objId
  )   
  ]

changeTime :: (Int -> Int) -> ByteString -> Int -> Int
changeTime fn x y = case B.readInt x of
  Just (r,"") -> fn r
  _ -> fn y

setService objId field val = do
  svcId <- get objId "parentId"
  set svcId field val

getService objId field
  = get objId "parentId"
  >>= (`get` field)
  

closeAction objId = do
  svcId <- get objId "parentId"
  kazeId <- get svcId "parentId"
  upd kazeId "actions" $ dropFromList objId
  set objId "closed" "1"

replaceAction actionName actionDesc targetGroup priority dueDelta objId = do
  assignee <- get objId "assignedTo"
  svcId <- get objId "parentId"
  due <- dateNow dueDelta
  kazeId <- get svcId "parentId"
  actionId <- new "action" $ Map.fromList
    [("name", actionName)
    ,("description", utf8 actionDesc)
    ,("targetGroup", targetGroup)
    ,("assignedTo", assignee)
    ,("priority", priority)
    ,("duetime", due)
    ,("parentId", svcId)
    ,("caseId", kazeId)
    ,("closed", "0")
    ]
  upd kazeId "actions" $ addToList actionId
  closeAction objId
  return actionId

requestFddsVin :: B.ByteString -> B.ByteString -> TriggerMonad b Bool
requestFddsVin objId vin = do
  let preparedVin = B.unpack $ B.map toUpper vin
  conf     <- lift $ gets fdds
  vinState <- liftIO Fdds.vinSearchInit
  result   <- liftIO (try $ Fdds.vinSearch conf vinState preparedVin
                      :: IO (Either SomeException [Fdds.Result]))
  case result of
    Right v -> return $ any (Fdds.rValid) v
    Left _  -> return False

setWeather objId city = do
  weather <- liftIO $ getWeather' (initApi "ru" "utf-8") $ BU.toString city
  case weather of
    Right w -> set objId "temperature" $ B.pack $ show $ tempC w
    Left  _ -> return ()
