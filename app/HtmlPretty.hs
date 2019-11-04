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
showRaw a = [shamlet| <pre>#{pp a}|]

showRaw' :: PP a => a -> Text -> Html
showRaw' a b = [shamlet| <pre class="#{b}">#{pp a}|]

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
  showHtml (I (Lexeme NoDirective a)) = [shamlet|#{showRaw' a "IntegerExpression"}|]
  showHtml (S (Lexeme NoDirective a)) = [shamlet|#{showRaw' a "StringExpression"}|]
  showHtml (DFalse) = [shamlet|<code>false|]
  showHtml (DTrue) = [shamlet|<code>true|]
  showHtml (Nil) = [shamlet|<code>Nil|]
  showHtml (V (Lexeme NoDirective a)) = [shamlet|#{showHtml a}|]
  showHtml a = [shamlet|<pre>#{showRaw' a "UnhandledExpression_ValueExpression"}|]

instance HtmlShow Expression where
  showHtml (a := b) = [shamlet|<code>
    <span class="lhs">#{showHtml a}
    <span class="op">=
    <span class="rhs">#{showHtml b}|]

  showHtml a = [shamlet|<pre>#{showRaw' a "UnhandledExpression_Expression"}|]


instance HtmlShow ConstDefinition where
  showHtml (ConstDefinition (Lexeme directives name) typ value) = [shamlet|
    $if isNoDirective directives
    $else
      #{showHtml directives}
    \ #{name}
    $maybe typ' <- typ
      \ : #{showHtml typ'}
    $nothing
    \ = #{showHtml value}
    \ |]
  showHtml a = [shamlet|
    #{showRaw a}
    |]

instance HtmlShow TypeName where
  showHtml (Type (Lexeme NoDirective a)) = [shamlet| #{showHtml a} |]
  showHtml (AddressOfType a b) = [shamlet|
    <span class="op">^<span>#{showHtml b}|]
  showHtml a = [shamlet| #{showRaw a} |]

instance HtmlShow TypeDefinition where
  showHtml (TypeDef a b) = [shamlet|#{showRaw' a "TypeDefinition_TypeDef"}: #{showRaw' b "TypeDefinition_TypeDef"}|]
  showHtml (TypeAlias a b) = [shamlet|<code>
    <span class="lhs">#{showHtml a}
    <span class="op"> = 
    <span class="rhs">#{showHtml b}|]
  showHtml a = [shamlet|#{showRaw' a "DefaultTypeDefinition"}|]

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
        <h2>Uses:
        #{showHtml a}
        <h3>Types Used from External Modules In Interface
        TODO
    <h2>Interface:
    <h3>Types
    #{showHtml (concat (mapMaybe types b))}
    <h3>Consts
    #{showHtml (concat (mapMaybe consts b))}
    <h3>Resources
    #{showHtml (concat (mapMaybe resources b))}
    <h3>Vars
    #{showHtml (concat (mapMaybe vars b))}
    <h3>Fields
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
  showHtml (Unit directive unitName intf impl init fin) =
    [shamlet|
        <h1>Unit: #{showHtml unitName}
        $if isNoDirective directive
        $else
          #{showHtml directive}

        #{showHtml intf}

        <h2>Implementation
        <h3>Uses
        <h3>Types Used from External Modules In Implementation
        TODO
    |]

  showHtml (Program a uses implSpecs b ) = [shamlet|
    <h1>Program
    #{showHtml a}
    -- Program listing omitted --
    |]
  showHtml (UnitFragment a b) = [shamlet|
    <h1>Fragment
    #{showHtml a}
    #{showHtml b}
    |]

