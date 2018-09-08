module DelphiAst where

import Data.Text (Text)

data Unit =
  Unit Text
       Interface
       Implementation
       Initialization
       Finalization
  deriving (Eq, Show)

data Interface =
  Interface Uses
            [InterfaceExpression]
  deriving (Eq, Show)

newtype Uses =
  Uses [Text]
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
                 [ImplementationSpec] -- Nested implementation specs.
                 Expression
  | ProcedureImpl TypeName
                  [Argument]
                  [FieldAnnotation]
                  [ImplementationSpec] -- Nested implementation specs.
                  Expression
  | AdditionalInterface InterfaceExpression
  | MemberFunctionImpl TypeName
                       TypeName
                       [Argument]
                       TypeName
                       [FieldAnnotation]
                       [ImplementationSpec] -- Nested implementation specs.
                       Expression
  | MemberConstructorImpl TypeName
                          TypeName
                          [Argument]
                          [FieldAnnotation]
                          [ImplementationSpec] -- Nested implementation specs.
                          Expression
  | MemberDestructorImpl TypeName
                         TypeName
                         [FieldAnnotation]
                         [ImplementationSpec] -- Nested implementation specs.
                         Expression
  | MemberProcedureImpl TypeName
                        TypeName
                        [Argument]
                        [FieldAnnotation]
                        [ImplementationSpec] -- Nested implementation specs.
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
  | With ValueExpression Expression
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
--  2) That a sensible error is provided if an unrecognised symbol is used.
data ValueExpression
  = V Text
  | T TypeName
  | I Integer
  | S Text
  | F Rational
  | L [ValueExpression] -- [foo, bar, baz]
  | P [ValueExpression] -- (foo, bar, baz)
  | LambdaFunction [Argument] TypeName [ImplementationSpec] Expression
  | LambdaProcedure [Argument] [ImplementationSpec] Expression
  | DTrue
  | DFalse
  | Result
  | Exit (Maybe ValueExpression)
  | Not ValueExpression
  | Inherited Text
  | Dereference ValueExpression -- '^foo'
  | AddressOf ValueExpression -- '@foo'
  | ValueExpression :& ValueExpression -- foo and bar
  | ValueExpression :| ValueExpression -- foo or bar
  | ValueExpression :== ValueExpression -- foo = bar
  | ValueExpression :+ ValueExpression -- foo + bar
  | ValueExpression :- ValueExpression -- foo - bar
  | ValueExpression :* ValueExpression -- foo * bar
  | ValueExpression :/ ValueExpression -- foo / bar
  | ValueExpression :<> ValueExpression -- foo <> bar
  | ValueExpression :< ValueExpression -- foo < bar
  | ValueExpression :<= ValueExpression -- foo <= bar
  | ValueExpression :>= ValueExpression -- foo >= bar 
  | ValueExpression :> ValueExpression -- foo > bar
  | As ValueExpression ValueExpression -- foo as bar
  | Is ValueExpression ValueExpression -- foo is bar
  | In ValueExpression ValueExpression -- foo in bar
  | ValueExpression :$ [ValueExpression] -- foo(bar, baz)
  | ValueExpression :!! [ValueExpression] -- foo[bar,baz]
  | ValueExpression :. ValueExpression -- foo.bar
  | ValueExpression :<<>> [TypeName] -- For generics
  | Nil -- nil
  deriving (Eq, Show)

data Initialization =
  Initialization
  deriving (Eq, Show)

data Finalization =
  Finalization
  deriving (Eq, Show)

-- These Type Definitions can only appear on the RHS.
data TypeDefinitionRHS
  = ReferenceToProcedure [Argument]
  | SimpleProcedure [Argument]
  | ProcedureOfObject [Argument]
  | ReferenceToFunction [Argument] TypeName
  | SimpleFunction [Argument] TypeName
  | FunctionOfObject [Argument] TypeName
  deriving (Eq, Show)

data TypeDefinition
  = TypeDef TypeName
            TypeDefinitionRHS
  | TypeAlias TypeName
              TypeName -- Simple type alias: type foo = bar;
  | EnumDefinition TypeName
                   [Text]
  | SetDefinition TypeName
                  TypeName
  | Record TypeName
           RecordDefinition
  | ForwardClass
  | TypeAttribute [ValueExpression] TypeDefinition
  | Class TypeName
          [TypeName]
          ClassDefinition
  deriving (Eq, Show)

data InterfaceExpression
  = TypeDefinitions [TypeDefinition]
  | ConstDefinitions [ConstDefinition]
  | VarDefinitions [VarDefinition]
  | Standalone Field -- TODO: Make this more specialised.
  deriving (Eq, Show)

data ConstDefinition =
  ConstDefinition Text
                  (Maybe TypeName)
                  ValueExpression
  deriving (Eq, Show)

data VarDefinition =
  VarDefinition Text
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
  = PropertyRead Text
  | PropertyWrite Text
  | PropertyStored
  | PropertyDefault ValueExpression -- TODO: Define a simpler set of "ValueExpression" that are limited to const.
  | PropertyNoDefault
  deriving (Eq, Show)

data Field
  = Constructor TypeName
                [Argument]
                [FieldAnnotation]
  | Field Name
          TypeName
  | Destructor TypeName
               [FieldAnnotation]
  | Procedure TypeName
              [Argument]
              [FieldAnnotation]
  | Function TypeName
             [Argument]
             TypeName
             [FieldAnnotation]
  | Property Text
             (Maybe [Argument])
             TypeName
             (Maybe ValueExpression)
             [PropertySpecifier]
             Bool
  | IndexProperty Name
                  (Maybe Argument)
                  TypeName
                  (Maybe Name)
                  (Maybe Name)
                  (Maybe Name)
                  (Maybe ValueExpression)
                  [FieldAnnotation]
  deriving (Eq, Show)

data FieldAnnotation
  = Override
  | Static -- Ie, a class function
  | Virtual
  | Overload
  | Reintroduce
  | Abstract
  | Default
  | StdCall
  | Message Text -- TODO: Encode all known windows messages?
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
      TypeName
      (Maybe ValueExpression)
  deriving (Eq, Show)

type RecordDefinition = [Accessibility]

type ClassDefinition = [Accessibility]

data ArrayIndex
  = IndexOf TypeName -- ie, Byte.  TODO: Consider how to constrain this to ordinals.
  | Range [(ValueExpression, ValueExpression)] -- ie, 34..56
  deriving (Eq, Show)

data TypeName
  = Type Text
  -- Arrays
  | StaticArray ArrayIndex
                TypeName
  | DynamicArray Integer
                 TypeName
  | VariantArray ArrayIndex
  | OpenDynamicArray TypeName
  | ConstType -- Eg, for an 'array of const'
  | AddressOfType TypeName -- '^'
  | TargetOfPointer TypeName -- '@'
  | Constraint [GenericConstraint]
  | GenericDefinition Text
                      [Argument]
  | GenericMethodOfType TypeName
                        TypeName
  | GenericInstance Text
                    [TypeName]
  | UnspecifiedType
  deriving (Eq, Show)

data GenericConstraint =
  ClassConstraint
  deriving (Eq, Show)

type ArgName = Text

type Name = Text

newtype Else =
  Else Expression
  deriving (Eq, Show)

newtype Then =
  Then Expression
  deriving (Eq, Show)
