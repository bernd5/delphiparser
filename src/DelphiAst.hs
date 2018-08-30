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
  Interface Uses [InterfaceExpression]
  deriving (Eq, Show)

newtype Uses = Uses [Text]
  deriving (Eq, Show)

data Implementation =
  Implementation Uses [ImplementationSpec]
  deriving (Eq, Show)

data ImplementationSpec
  = FunctionImpl TypeName
                 [Argument]
                 TypeName
                 Expression
  | AdditionalInterface InterfaceExpression
  | MemberFunctionImpl TypeName
                       TypeName
                       [Argument]
                       TypeName
                       Expression
  | MemberConstructorImpl TypeName
                          TypeName
                          [Argument]
                          Expression
  | MemberDestructorImpl TypeName
                         TypeName
                         Expression
  | MemberProcedureImpl TypeName
                        TypeName
                        [Argument]
                        Expression
  deriving (Eq, Show)

data LoopDirection = LoopUpTo | LoopDownTo
  deriving (Eq, Show)

data Expression
  = Expr Text
  | ValueExpression := ValueExpression     -- foo := bar
  | If ValueExpression
       Then
       Else
  | For Expression LoopDirection ValueExpression Expression
  | While ValueExpression Expression
  | Repeat [Expression] ValueExpression
  | Begin [Expression]
  | ExpressionValue ValueExpression
  | EmptyExpression
  deriving (Eq, Show)

-- ValueExpression are expressions that result in a value when evaluated
data ValueExpression
  = V Text
  | T TypeName
  | I Integer
  | S Text
  | DTrue
  | DFalse
  | Result
  | Not ValueExpression
  | Dereference ValueExpression -- '^foo'
  | AddressOf ValueExpression -- '@foo'
  | ValueExpression :& ValueExpression     -- foo and bar
  | ValueExpression :| ValueExpression     -- foo or bar
  | ValueExpression :== ValueExpression     -- foo = bar
  | ValueExpression :+ ValueExpression     -- foo + bar
  | ValueExpression :- ValueExpression     -- foo - bar
  | ValueExpression :* ValueExpression     -- foo * bar
  | ValueExpression :/ ValueExpression     -- foo / bar
  | ValueExpression :<> ValueExpression    -- foo <> bar
  | ValueExpression :< ValueExpression    -- foo < bar
  | ValueExpression :<= ValueExpression    -- foo <= bar
  | ValueExpression :> ValueExpression    -- foo > bar
  | ValueExpression `As` ValueExpression   -- foo as bar
  | ValueExpression :$  [ValueExpression]  -- foo(bar, baz)
  | ValueExpression :!! [ValueExpression]  -- foo[bar,baz]
  | ValueExpression :.  ValueExpression    -- foo.bar
  | ValueExpression :<<>> [TypeName] -- For generics
  | Nil                                    -- nil
  deriving (Eq, Show)

data Initialization =
  Initialization
  deriving (Eq, Show)

data Finalization =
  Finalization
  deriving (Eq, Show)

-- These Type Definitions can only appear on the RHS.
data TypeDefinitionRHS
  = UnknownTypeDefinition Text
  | ReferenceToProcedure [Argument]
  deriving (Eq, Show)

data TypeDefinition
  = TypeDef TypeName
            TypeDefinitionRHS
  | TypeAlias TypeName TypeName -- Simple type alias: type foo = bar;
  | EnumDefinition TypeName [Text]
  | SetDefinition TypeName TypeName
  | Record TypeName
           RecordDefinition
  | ForwardClass
  | Class TypeName
          [TypeName]
          ClassDefinition
  deriving (Eq, Show)

data InterfaceExpression
  = TypeDefinitions [TypeDefinition]
  | ConstDefinitions [ConstDefinition]
  | VarDefinitions [VarDefinition]
  deriving (Eq, Show)

data ConstDefinition
  = ConstDefinition Text (Maybe TypeName) [ValueExpression]
  deriving (Eq, Show)

data VarDefinition
  = VarDefinition Text TypeName
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
  | PropertyDefault Text
  | PropertyNoDefault
  deriving (Eq, Show)

data Field
  = Constructor Name
                [Argument]
  | Field Name
          TypeName
  | Destructor Name
               [Annotation]
  | Procedure Name
              [Argument]
              [Annotation]
  | Function TypeName
             [Argument]
             TypeName
             [Annotation]
  | Property Text (Maybe [Argument]) TypeName (Maybe Text) [PropertySpecifier] Bool
  | IndexProperty Name
                  (Maybe Argument)
                  TypeName
                  (Maybe Name)
                  (Maybe Name)
                  (Maybe Name)
                  (Maybe ValueExpression)
                  [Annotation]
  deriving (Eq, Show)

data Annotation
  = Override
  | Virtual
  | Default
  | StdCall
  deriving (Eq, Show)

data Argument =
  Arg ArgName
      TypeName
  deriving (Eq, Show)

type RecordDefinition = [Accessibility]

type ClassDefinition = [Accessibility]

data ArrayIndex
  = IndexOf TypeName -- ie, Byte.  TODO: Consider how to constrain this to ordinals.
  | Range [(Integer,Integer)] -- ie, 34..56
  deriving (Eq, Show)

data TypeName
  = Type Text
  -- Arrays
  | StaticArray ArrayIndex TypeName
  | DynamicArray Integer TypeName
  | VariantArray ArrayIndex
  | OpenDynamicArray TypeName
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
