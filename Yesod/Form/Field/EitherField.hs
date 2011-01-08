{-# LANGUAGE QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           , OverloadedStrings
           #-}
module Yesod.Form.Field.EitherField where

import Yesod
import Yesod.Form
import Yesod.Form.Core
import Data.Monoid

eitherField :: (Show a, Show b) 
            => FormFieldSettings 
            -> FormletField s m a
            -> FormletField s m b
            -> Either Int (Either a b) 
            -> FormField s m (Either a b)
eitherField ffs fl1 fl2 orig = GForm $ do
  env <- askParams
  let label = ffsLabel ffs
      tooltip = ffsTooltip ffs
  name <- maybe newFormIdent return $ ffsName ffs
  ident <- maybe newFormIdent return $ ffsId ffs
  lRadioId <- newFormIdent
  rRadioId <- newFormIdent
  onClickFn <- newFormIdent

  let (defaultF1Val, defaultF2Val) = 
        case orig of 
             Left _ -> (Nothing, Nothing)
             Right e -> 
               case e of
                    Left l -> (Just l, Nothing)
                    Right r -> (Nothing, Just r)

      (GForm f1) = fl1 defaultF1Val
      (GForm f2) = fl2 defaultF2Val

  (rl, [il], el) <- f1
  (rr, [ir], er) <- f2

  let leftId = fiIdent il
      rightId = fiIdent ir

      defaultCheckedField =
        case orig of
             Left 0 -> leftId
             Left 1 -> rightId
             Left _ -> leftId
             Right (Left _) -> leftId
             Right _ -> rightId

      onClickW = addJulius [$julius|
          function %onClickFn%(id) {
            $('#%leftId%').hide();
            $('#%rightId%').hide();
            $('#' + id).show();
          }
        |]

      clearErrors (FormFailure _) = FormFailure []
      clearErrors x = x

      (res, checkedField) = 
        if null env
           then (FormMissing, defaultCheckedField)
           else case lookup name env of
                     Nothing -> (FormMissing, defaultCheckedField)
                     Just a | a == leftId -> (clearErrors $ fmap Left rl, leftId)
                            | a == rightId -> (clearErrors $ fmap Right rr, rightId)
                            | otherwise -> (FormFailure ["Invalid Entry"], defaultCheckedField)
      fiChecked fi = fiIdent fi == checkedField
      radioIdent fi = case fiIdent fi of
                          ident | ident == leftId -> lRadioId
                                | ident == rightId -> rRadioId
      clearLabel fi = fi { fiLabel = "" }

      clearFieldErrors fi = if fiChecked fi
                               then fi
                               else fi { fiErrors = Nothing }

      mkRadio fi = [$hamlet|
        .
          %input!:fiChecked fi:checked!type=radio!id=$radioIdent fi$!name=$name$!value=$fiIdent fi$!onClick=$onClickFn$('$fiIdent fi$')
          %label!for=$fiIdent.fi$ $fiLabel fi$
            .tooltip $fiTooltip.fi$
        |]

      mkInput fi = [$hamlet|
        .!id=$fiIdent fi$
          ^fiInput fi^
          $maybe fiErrors.clearFieldErrors.fi err
            %div.errors $err$
        |]
          
  let input = do
        onClickW
        addJulius [$julius|
          $().ready(function () {
            %onClickFn%('%checkedField%');
          });
        |]

        [$hamlet| 
          %table!id=$ident$
            %tr
              %th!align=left
                ^mkRadio il^
                ^mkRadio ir^
              %td
                ^mkInput il^
                ^mkInput ir^
        |]
      fi = FieldInfo 
           { fiLabel = string label
           , fiTooltip = tooltip
           , fiIdent = ident
           , fiInput = input
           , fiErrors = Nothing                                
           , fiRequired = True
           }
  return (res, [fi], mappend el er)


