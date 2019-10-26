{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module HtmlPretty where

import Yesod hiding (Field)
import DelphiAst
import AstPrettyPrint (PP(..))
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Text.Blaze.Html.Renderer.String (renderHtml)


notImplemented = [shamlet|<span>Not Implemented|]
showRaw :: PP a => a -> Html
showRaw a = [shamlet| <code> #{pp a}|]

class HtmlShow a where
  showHtml :: a -> Html

instance HtmlShow Text where
  showHtml a = [shamlet|#{a}|]

instance HtmlShow a => HtmlShow [a] where
  showHtml a = [shamlet|
  $if null a
  $else
    <ul>
      $forall directive <- a
        <li>#{showHtml directive}</li>
  |]

instance HtmlShow a => HtmlShow (Maybe a) where
  showHtml (Just a) = showHtml a
  showHtml Nothing = [shamlet||]

instance HtmlShow ValueExpression where
  showHtml (I (Lexeme NoDirective a)) = [shamlet|#{showRaw a}|]
  showHtml (S (Lexeme NoDirective a)) = [shamlet|#{showRaw a}|]
  showHtml (DFalse) = [shamlet|<code>false|]
  showHtml (DTrue) = [shamlet|<code>true|]
  showHtml a = [shamlet|#{showRaw a}|]

instance HtmlShow ConstDefinition where
  showHtml (ConstDefinition (Lexeme directives name) typ value) = [shamlet|
    $if isNoDirective directives
    $else
      #{showHtml directives}
    \ #{name}
    $maybe typ' <- typ
      \ : #{showHtml typ'}
    $nothing
    \ = #{showRaw value}
    \ |]
  showHtml a = [shamlet|
    #{showRaw a}
    |]

instance HtmlShow TypeName where
  showHtml (Type (Lexeme NoDirective a)) = [shamlet| #{showHtml a} |]
  showHtml a = [shamlet| #{showRaw a} |]

instance HtmlShow TypeDefinition where
  showHtml a = [shamlet| #{showRaw a}|]

instance HtmlShow VarDefinition where
  showHtml (VarDefinition a b c) = [shamlet|
    #{showHtml a}
    #{showRaw b}
    #{showRaw c}
    |]
  showHtml a = [shamlet|#{showRaw a}|]

instance HtmlShow Field where
  showHtml a = [shamlet|#{showRaw a}|]

instance HtmlShow InterfaceExpression where
  showHtml (TypeDefinitions a) = [shamlet| #{showHtml a} |]
  showHtml (ConstDefinitions a) = [shamlet| #{showHtml a} |]
  showHtml (ResourceDefinitions a) = [shamlet| #{showHtml a} |]
  showHtml (VarDefinitions a) = [shamlet| #{showHtml a} |]
  showHtml (Standalone a) = [shamlet| #{showHtml a} |]

instance HtmlShow Interface where
  showHtml (Interface (Uses a _) b) = [shamlet|
    $if null a
    $else
      <div class="uses">
        <h1>Uses:
        #{showHtml a}
    <h1>Interface:
    <h2>Types
    #{showHtml (concat (mapMaybe types b))}
    <h2>Consts
    #{showHtml (concat (mapMaybe consts b))}
    <h2>Resources
    #{showHtml (concat (mapMaybe resources b))}
    <h2>Vars
    #{showHtml (concat (mapMaybe vars b))}
    <h2>Fields
    #{showHtml (mapMaybe fields b)}
    |]
    where
      types (TypeDefinitions s) = Just s
      types _ = Nothing

      consts (ConstDefinitions s) = Just s
      consts _ = Nothing

      resources (ResourceDefinitions s) = Just s
      resources _ = Nothing

      vars (VarDefinitions s) = Just s
      vars _ = Nothing

      fields s@(Standalone _) = Just s
      fields _ = Nothing

instance HtmlShow a => HtmlShow (Lexeme a) where
  showHtml (Lexeme a b) = [shamlet|
  #{showHtml b}
  $if isNoDirective a
  $else
    #{showHtml a}
  |]

instance HtmlShow Directive where
  showHtml (Comment a) = [shamlet|<pre>#{a}|]
  showHtml (Compound a b) = [shamlet|#{showHtml a}#{showHtml b}|]
  showHtml a = [shamlet|<pre>#{show a}|]

isNoDirective NoDirective = True
isNoDirective _ = False

instance HtmlShow Unit where
  showHtml (Unit a b c d e f) = [shamlet|
  <h1>Unit: #{showHtml b}
  $if isNoDirective a
  $else
    #{showHtml a}

  #{showHtml c}
  |]
  showHtml (Program a b) = [shamlet|
    <h1>Program
    #{showHtml a}
    -- Program listing omitted --
    |]
  showHtml (UnitFragment a b) = [shamlet|
    <h1>Fragment
    #{showHtml a}
    #{showHtml b}
    |]
