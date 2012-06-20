{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Templates.Index (index) where

import Text.Hamlet
import Data.Text (Text)

import Templates.Fields

layout content = $(hamletFile "src/Templates/layout.hamlet")

navbar :: Text -> t -> Html
navbar user = $(hamletFile "src/Templates/navbar.hamlet")

callScreen       = $(hamletFile "src/Templates/screens/call.hamlet")
caseScreen       = $(hamletFile "src/Templates/screens/case.hamlet")
searchScreen     = $(hamletFile "src/Templates/screens/search.hamlet")
backofficeScreen = $(hamletFile "src/Templates/screens/backoffice.hamlet")
reportsScreen    = $(hamletFile "src/Templates/screens/reports.hamlet")
vinScreen        = $(hamletFile "src/Templates/screens/vin.hamlet")
partnerScreen    = $(hamletFile "src/Templates/screens/partner.hamlet")

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
            ^{allFields}
            |]