{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , OverloadedStrings
           , TypeFamilies
           #-}

import Yesod
import Yesod.Form.Field.EitherField
import Control.Applicative

data Test = Test

data Params = Params
  { eitherIntString :: Either Int String
  }
  deriving (Show)

paramsFormlet :: Maybe Params -> Form s m Params
paramsFormlet mparams = fieldsToTable $ Params
  <$> eitherField "Either Int Or String" 
                  (intField "int")
                  (stringField "string") 
                  (maybe (Left 0) Right $ fmap eitherIntString mparams)

getRootR :: GHandler Test Test RepHtml
getRootR = do
  (res,w,enc) <- runFormGet $ paramsFormlet $ Just $ Params $ Left 0 -- Right "hey"
  defaultLayout $ do
    [$hamlet|
$show res$
%form!enctype=$enc$!method=GET
  %table
    ^w^
    %tr
      %td!colspan=2
        %input!type=submit
|]

mkYesod "Test" [$parseRoutes|
/ RootR GET
|]

instance Yesod Test where approot _ = ""

main = basicHandler 3002 Test
