{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module DelphiAst where

import Data.Text (Text)

-- This is the crude lexeme - a single "word" in the delphi code.
data Directive
  = Comment Text
  | Include Text
  | IfDef Text [Either Directive Text] [Either Directive Text]
  | UnknownDirective (Text, Text)
  | Compound Directive Directive
  | NoDirective
  deriving (Eq, Show)

instance Semigroup Directive where
  a <> NoDirective = a
  NoDirective <> a = a
  a <> Compound b c = Compound (Compound a b) c
  a <> b = Compound a b

instance Monoid Directive where
  mempty = NoDirective

data Lexeme a = Lexeme Directive a
  deriving (Eq, Show)

instance (Functor Lexeme) where
  --fmap :: (a -> b) -> f a -> f b
  fmap f (Lexeme a b) = Lexeme a (f b)


instance Semigroup a => (Semigroup (Lexeme a)) where
  (Lexeme a b) <> (Lexeme c d) = Lexeme (a <> c) (b <> d)

data Unit = Unit
             Directive
             (Lexeme Text)
             Interface
             Implementation
             Initialization
             Finalization
          | Program (Lexeme Text) Uses [ImplementationSpec] [Expression]
          | UnitFragment Directive Text
  deriving (Eq, Show)

data Interface =
  Interface Uses
            [InterfaceExpression]
  deriving (Eq, Show)

data Uses =
  Uses [Lexeme Text] Directive
  deriving (Eq, Show)

data Implementation =
  Implementation Uses
                 [ImplementationSpec]
  deriving (Eq, Show)

data ImplementationSpec
  = FunctionImpl TypeName
                 [Argument]
                 TypeName
                 [FieldAnnotation]
                 [ImplementationSpec] -- Nested implementation specs
                 Expression
  | ProcedureImpl TypeName
                  [Argument]
                  [FieldAnnotation]
                  [ImplementationSpec] -- Nested implementation specs
                  Expression
  | AdditionalInterface InterfaceExpression
  | MemberFunctionImpl TypeName
                       TypeName
                       [Argument]
                       TypeName
                       [FieldAnnotation]
                       [ImplementationSpec] -- Nested implementation specs
                       Expression
  | MemberConstructorImpl TypeName
                          TypeName
                          [Argument]
                          [FieldAnnotation]
                          [ImplementationSpec] -- Nested implementation specs
                          Expression
  | MemberDestructorImpl TypeName
                         TypeName
                         [FieldAnnotation]
                         [ImplementationSpec] -- Nested implementation specs
                         Expression
  | MemberProcedureImpl TypeName
                        TypeName
                        [Argument]
                        [FieldAnnotation]
                        [ImplementationSpec] -- Nested implementation specs
                        Expression
  deriving (Eq, Show)

data LoopDirection
  = LoopUpTo
  | LoopDownTo
  deriving (Eq, Show)

data Except
   = ExceptOn (Maybe Argument) [Expression]
   | ExceptElse [Expression]
  deriving (Eq, Show)

type Finally = [Expression]

data CaseBranches =
  CaseBranch [ValueExpression]
             Expression
  deriving (Eq, Show)

data Expression -- TODO: Should be 'Statement'
  = Expr Text
  | ValueExpression := ValueExpression -- foo := bar
  | If ValueExpression
       Then
       Else
  | Raise ValueExpression
  | Try [Expression]
        (Either [Except] Finally)
  | Case ValueExpression
         [CaseBranches]
         (Maybe Else)
  | For Expression
        LoopDirection
        ValueExpression
        Expression
  | ForIn ValueExpression
        ValueExpression
        Expression
  | Break
  | Continue
  | With [ValueExpression] Expression -- "with" can have multiple names in fpc
  | While ValueExpression
          Expression
  | Repeat [Expression]
           ValueExpression
  | Begin [Expression]
  | ExpressionValue ValueExpression
  | EmptyExpression
  deriving (Eq, Show)

-- ValueExpression are expressions that result in a value when evaluated
-- TODO: Ensure that:
--  1) Spaces aren't required, and
--  2) That a sensible error is provided if an unrecognised symbol is used
data ValueExpression
  = V (Lexeme Text)
  | T TypeName
  | I (Lexeme Integer)
  | S (Lexeme Text)
  | F Rational
  | L [ValueExpression] -- [foo, bar, baz]
  | P [ValueExpression] -- (foo, bar, baz)
  | A ArrayIndex
  | RecordValue [Expression]
  | LambdaFunction [Argument] TypeName [ImplementationSpec] Expression
  | LambdaProcedure [Argument] [ImplementationSpec] Expression
  | DTrue
  | DFalse
  | Result
  | Exit (Maybe ValueExpression)
  | Not ValueExpression
  | Negate ValueExpression
  | Inherited (Maybe (Lexeme Text))
  | Dereference ValueExpression -- '^foo'
  | AddressOf ValueExpression -- '@foo'
  | ToChar ValueExpression -- #42
  | ValueExpression :& ValueExpression -- foo and bar
  | ValueExpression :| ValueExpression -- foo or bar
  | ValueExpression :== ValueExpression -- foo = bar
  | ValueExpression :--:-- ValueExpression -- WriteLn(foo:24)
  | ValueExpression :=. ValueExpression -- foo := bar (TODO: Remove this one)
  | ValueExpression :+ ValueExpression -- foo + bar
  | ValueExpression :- ValueExpression -- foo - bar
  | ValueExpression :* ValueExpression -- foo * bar
  | ValueExpression :/ ValueExpression -- foo / bar
  | ValueExpression :% ValueExpression -- foo mod bar
  | ValueExpression :<> ValueExpression -- foo <> bar
  | ValueExpression :< ValueExpression -- foo < bar
  | ValueExpression :<= ValueExpression -- foo <= bar
  | ValueExpression :>= ValueExpression -- foo >= bar
  | ValueExpression :>> ValueExpression -- foo >> bar
  | ValueExpression :<< ValueExpression -- foo << bar
  | ValueExpression :> ValueExpression -- foo > bar
  | As ValueExpression ValueExpression -- foo as bar
  | Is ValueExpression ValueExpression -- foo is bar
  | In ValueExpression ValueExpression -- foo in bar
  | ValueExpression :$ [ValueExpression] -- foo(bar, baz)
  | ValueExpression :!! [ValueExpression] -- foo[bar,baz]
  | ValueExpression :. ValueExpression -- foo.bar
  | ValueExpression :.. ValueExpression -- foo..bar
  | ValueExpression :<<>> [TypeName] -- For generics
  | Nil -- nil
  deriving (Eq, Show)

data Initialization =
  Initialization
  deriving (Eq, Show)

data Finalization =
  Finalization
  deriving (Eq, Show)

-- These Type Definitions can only appear on the RHS
data TypeDefinitionRHS
  = ReferenceToProcedure [Argument]
  | SimpleProcedure [Argument]
  | ProcedureOfObject [Argument]
  | ReferenceToFunction [Argument] TypeName
  | SimpleFunction [Argument] TypeName
  | NestedFunction [Argument] TypeName
  | FunctionOfObject [Argument] TypeName
  | NewType TypeName -- ie, foo = type bar
  | ClassOf TypeName -- ie, 'foo = class of bar'
  | ClassHelper TypeName ClassDefinition
  deriving (Eq, Show)

data TypeDefinition
  = TypeDef TypeName
            TypeDefinitionRHS
  | TypeAlias TypeName
              TypeName -- Simple type alias: type foo = bar;
  | EnumDefinition TypeName
                   [Lexeme Text]
  | SetDefinition TypeName
                  TypeName
  | Record TypeName
           RecordDefinition
  | ForwardClass TypeName
  | TypeAttribute [ValueExpression] TypeDefinition
  | InterfaceType TypeName [TypeName] ClassDefinition
  | Class TypeName
          [TypeName]
          ClassDefinition
  | TypeExpression ValueExpression
  deriving (Eq, Show)

data InterfaceExpression
  = TypeDefinitions [TypeDefinition]
  | ConstDefinitions [ConstDefinition]
  | ResourceDefinitions [ConstDefinition]
  | VarDefinitions [VarDefinition]
  | Standalone Field -- TODO: Make this more specialised
  deriving (Eq, Show)

data ConstDefinition
  = ConstDefinition (Lexeme Text)
                  (Maybe TypeName)
                  ValueExpression
  | ConstDirectiveFragment (Lexeme Text) (Maybe TypeName) Directive
  deriving (Eq, Show)

data VarDefinition =
  VarDefinition (Lexeme Text)
                TypeName
                (Maybe ValueExpression)
  deriving (Eq, Show)

data Accessibility
  = Private [Field]
  | Public [Field]
  | Protected [Field]
  | Published [Field]
  | DefaultAccessibility [Field]
  deriving (Eq, Show)

data PropertySpecifier
  = PropertyRead [Lexeme Text]
  | PropertyWrite [Lexeme Text]
  | PropertyStored
  | PropertyDefault ValueExpression -- TODO: Define a simpler set of "ValueExpression" that are limited to const
  | PropertyNoDefault
  deriving (Eq, Show)

data Field
  = Constructor TypeName
                [Argument]
                [FieldAnnotation]
  | Field Name
          TypeName
  | ClassVar TypeName TypeName
  | Destructor TypeName
               [FieldAnnotation]
  | Procedure TypeName
              [Argument]
              [FieldAnnotation]
  | Function TypeName
             [Argument]
             TypeName
             [FieldAnnotation]
  | Property (Lexeme Text)
             (Maybe [Argument])
             TypeName
             (Maybe ValueExpression)
             [PropertySpecifier]
             Bool
  | InheritedProperty (Lexeme Text) -- Ie, just "property foo;"
  | InheritedFunction (Lexeme Text) -- Ie, just "function foo;"
  | RedirectedFunction (Lexeme Text) (Lexeme Text) -- ie, "function IFoo.bar= newBar"
  | IndexProperty Name
                  (Maybe Argument)
                  TypeName
                  (Maybe Name)
                  (Maybe Name)
                  (Maybe Name)
                  (Maybe ValueExpression)
                  [FieldAnnotation]
  | CaseField ValueExpression
         [([ValueExpression], [Field])]
         (Maybe [Field])
  deriving (Eq, Show)

data FieldAnnotation
  = Override
  | Static -- Ie, a class function
  | Virtual
  | NoReturn
  | Inline
  | Final
  | Dynamic
  | Overload
  | Reintroduce
  | Abstract
  | Default
  | StdCall
  | Message (Lexeme Text) -- TODO: Encode all known windows messages?
  deriving (Eq, Show)

data ArgModifier
  = ConstArg
  | VarArg
  | OutArg
  | NormalArg
  deriving (Eq, Show)

data Argument =
  Arg ArgModifier
      ArgName
      (Maybe TypeName) -- Don't forget that untyped arguments exist!
      (Maybe ValueExpression)
  deriving (Eq, Show)

type RecordDefinition = [Accessibility]

type ClassDefinition = [Accessibility]

newtype ArrayIndex
  = IndexOf [ValueExpression]
  deriving (Eq, Show)

data TypeName
  = Type (Lexeme Text)
  | DirectiveType (Lexeme TypeName)
  -- Arrays
  | StaticArray ArrayIndex
                TypeName
  | DynamicArray Integer
                 TypeName
  | VariantArray ArrayIndex
  | Set TypeName
  | OpenDynamicArray TypeName
  | ConstType -- Eg, for an 'array of const'
  | AddressOfType Directive TypeName -- '^'
  | TargetOfPointer Directive TypeName -- '@'
  | Constraint [GenericConstraint]
  | GenericDefinition (Lexeme Text)
                      [Argument]
  | GenericMethodOfType TypeName
                        TypeName
  | GenericInstance (Lexeme Text)
                    [TypeName]
  | UnspecifiedType
  | UnspecifiedType' Text TypeName
  deriving (Eq, Show)

data GenericConstraint =
  ClassConstraint
  deriving (Eq, Show)

type ArgName = Lexeme Text

type Name = Lexeme Text

newtype Else =
  Else Expression
  deriving (Eq, Show)

newtype Then =
  Then Expression
  deriving (Eq, Show)
