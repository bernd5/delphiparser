module DelphiAst where

import Data.Text (Text)

data Unit =
  Unit Text
       Interface
       Implementation
       Initialization
       Finalization
  deriving (Eq, Show)

newtype Interface =
  Interface [InterfaceExpression]
  deriving (Eq, Show)

newtype Implementation =
  Implementation [ImplementationSpec]
  deriving (Eq, Show)

data ImplementationSpec
  = FunctionImpl TypeName
                 [Argument]
                 TypeName
                 Expression
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

data Expression
  = Expr Text
  | ValueExpression := ValueExpression     -- foo := bar
  | If ValueExpression
       Then
  | Begin [Expression]
  | ExpressionValue ValueExpression
  | EmptyExpression
  deriving (Eq, Show)

-- ValueExpression are expressions that result in a value when evaluated
data ValueExpression
  = V Text
  | T TypeName
  | I Integer
  | ValueExpression :& ValueExpression     -- foo and bar
  | ValueExpression :+ ValueExpression     -- foo + bar
  | ValueExpression :- ValueExpression     -- foo - bar
  | ValueExpression :* ValueExpression     -- foo * bar
  | ValueExpression :/ ValueExpression     -- foo / bar
  | ValueExpression :<> ValueExpression    -- foo <> bar
  | ValueExpression :< ValueExpression    -- foo < bar
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

data InterfaceExpression
  = TypeDef TypeName
            TypeDefinition
  | Record TypeName
           RecordDefinition
  | Class TypeName
          [TypeName]
          ClassDefinition
  deriving (Eq, Show)

data Accessibility
  = Private [Field]
  | Public [Field]
  | Protected [Field]
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
  | IndexProperty Name
                  Argument
                  TypeName
                  (Maybe Name)
                  (Maybe Name)
                  [Annotation]
  deriving (Eq, Show)

data Annotation
  = Override
  | Virtual
  | Default
  deriving (Eq, Show)

data TypeDefinition
  = UnknownTypeDefinition Text
  | ReferenceToProcedure [Argument]
  deriving (Eq, Show)

data Argument =
  Arg ArgName
      TypeName
  deriving (Eq, Show)

type RecordDefinition = [Accessibility]

type ClassDefinition = [Accessibility]

data TypeName
  = Type Text
  | Array TypeName
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

newtype Then =
  Then Expression
  deriving (Eq, Show)
