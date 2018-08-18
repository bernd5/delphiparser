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
  = FunctionImpl Name
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
                         [Argument]
                         Expression
  | MemberProcedureImpl TypeName
                        TypeName
                        [Argument]
                        Expression
  deriving (Eq, Show)

data Expression
  = Expr Text
  | Assign ValueExpression
           ValueExpression
  | If ValueExpression
       Then
  | Begin [Expression]
  | ExpressionValue ValueExpression
  | EmptyExpression
  deriving (Eq, Show)

-- ValueExpression are expressions that result in a value when evaluated
data ValueExpression
  = SimpleReference Text
  | Integer Integer
  | TypeMemberRef TypeName
                  TypeName
                  [ValueExpression]
  | Operation ValueExpression
              Text
              ValueExpression
  | FunctionCall ValueExpression
                 [ValueExpression]
  | IndexCall ValueExpression
              [ValueExpression]
  | Nil
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
  | Function Name
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
