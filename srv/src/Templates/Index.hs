{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Templates.Index (index) where

import Text.Hamlet
import Data.Text (Text)

import Templates.Fields

layout content = $(hamletFile "resources/templates/layout.hamlet")

navbar :: Text -> t -> Html
navbar user = $(hamletFile "resources/templates/navbar.hamlet")

callScreen       = $(hamletFile "resources/templates/screens/call.hamlet")
caseScreen       = $(hamletFile "resources/templates/screens/case.hamlet")
searchScreen     = $(hamletFile "resources/templates/screens/search.hamlet")
backofficeScreen = $(hamletFile "resources/templates/screens/backoffice.hamlet")
reportsScreen    = $(hamletFile "resources/templates/screens/reports.hamlet")
vinScreen        = $(hamletFile "resources/templates/screens/vin.hamlet")
partnerScreen    = $(hamletFile "resources/templates/screens/partner.hamlet")

mainCont = [hamlet|
            <!-- Main container for dynamically rendered layouts -->
            <div class="container-fluid">
                     <div class="row-fluid" id="layout">
            |]

index user =
    layout [hamlet|
            ^{navbar user}
            ^{mainCont}
            ^{callScreen}
            ^{caseScreen}
            ^{searchScreen}
            ^{backofficeScreen}
            ^{reportsScreen}
            ^{vinScreen}
            ^{partnerScreen}
            |]