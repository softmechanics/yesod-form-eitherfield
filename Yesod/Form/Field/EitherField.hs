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
  onClickFn <- newFormIdent

  let (defaultF1Val, defaultF2Val) = 
        case orig of 
             Left _ -> (Nothing, Nothing)
             Right e -> 
               case e of
                    Left l -> (Just l, Nothing)
                    Right r -> (Nothing, Just r)

  (rl, [il], el) <- deform $ fl1 defaultF1Val
  (rr, [ir], er) <- deform $ fl2 defaultF2Val

  let leftId = fiIdent il
      rightId = fiIdent ir

  let defaultCheckedField =
        case orig of
             Left 0 -> leftId
             Left 1 -> rightId
             Left _ -> leftId
             Right (Left _) -> leftId
             Right _ -> rightId

  -- Don't report errors at the level of the radio buttons.
  -- They're already reported by the inner fields
  let clearErrors (FormFailure _) = FormFailure []
      clearErrors x = x

  let (res, checkedField) = 
        case lookup name env of
             Nothing -> (FormMissing, defaultCheckedField)
             Just a | a == leftId -> (clearErrors $ fmap Left rl, leftId)
                    | a == rightId -> (clearErrors $ fmap Right rr, rightId)
                    | otherwise -> (FormFailure ["Invalid Entry"], defaultCheckedField)

  let fiChecked fi = fiIdent fi == checkedField

      -- We render the label near the radio button, so don't want to render it again with the field
      clearLabel fi = fi { fiLabel = "" }

      -- Don't show errors for the unchecked field
      clearFieldErrors fi = if fiChecked fi then fi else fi { fiErrors = Nothing }

  let mkRadio fi = [$hamlet|
        .
          %input!:fiChecked fi:checked!type=radio!name=$name$!value=$fiIdent fi$!onClick=$onClickFn$('$fiIdent fi$')
          %label!for=$fiIdent.fi$ $fiLabel fi$
            .tooltip $fiTooltip.fi$
        |]

  let mkInput fi = [$hamlet|
        .!id=$fiIdent fi$
          ^fiInput fi^
          $maybe fiErrors.clearFieldErrors.fi err
            %div.errors $err$
        |]
          
  let input = do
        addJulius [$julius|
          function %onClickFn%(id) {
            $('#%leftId%').hide();
            $('#%rightId%').hide();
            $('#' + id).show();
          }

          $().ready(function () {
            %onClickFn%('%checkedField%');
          });
        |]

        addWidget [$hamlet| 
          %table!id=$ident$
            %tr
              %th!align=left
                ^mkRadio il^
                ^mkRadio ir^
              %td
                ^mkInput il^
                ^mkInput ir^
        |]
  let fi = FieldInfo 
           { fiLabel = string label
           , fiTooltip = tooltip
           , fiIdent = ident
           , fiInput = input
           , fiErrors = Nothing                                
           , fiRequired = True
           }
  return (res, [fi], mappend el er)


