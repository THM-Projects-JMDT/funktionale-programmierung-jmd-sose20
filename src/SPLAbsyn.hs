{-|
Module      : SPLAbsyn
Description : Abstract syntax

This module provides the SPL-AST types. These AST types differ from ordinary AST types, as they have a "slot" 
for comments and empty lines appearing in the source code.
-}

module SPLAbsyn where


-- |Type alias for an AST node along with the comments belonging to it. 
--
-- The location of the comments in the source code determines the position in the list. This is useful for the pretty printing step.
type Commented a = (a, [[Comment]])

type Comment = String

type IntString = String

data Op = Lt | Ne | Asgn | Plus | Slash | Star | Gt | Le | Minus | Ge | Eq
          deriving (Eq, Show)

-- * Abstract Syntax
data Program              = Program [Commented GlobalDeclaration]
                          deriving (Eq, Show)
data GlobalDeclaration    = TypeDeclaration String (Commented TypeExpression)
                          | ProcedureDeclaration String [Commented ParameterDeclaration] [Commented VariableDeclaration] [Commented Statement]
                          | GlobalComment Comment
                          | GlobalEmptyLine
                          deriving (Eq, Show)
data TypeExpression       = ArrayTypeExpression IntString (Commented TypeExpression)
                          | NamedTypeExpression String
                          deriving (Eq, Show)
data ParameterDeclaration = ParameterDeclaration String (Commented TypeExpression) Bool
                          deriving (Eq, Show)
data VariableDeclaration  = VariableDeclaration String (Commented TypeExpression)
                          | VariableDeclarationComment Comment
                          deriving (Eq, Show)
data Statement            = AssignStatement (Commented Variable) (Commented Expression)
                          | CallStatement String [Commented Expression]
                          | CompoundStatement [Commented Statement]
                          | EmptyStatement
                          | IfStatement (Commented Expression) (Commented Statement) (Maybe (Commented Statement))
                          | WhileStatement (Commented Expression) (Commented Statement)
                          | StatementComment Comment
                          | StatementEmptyLine
                          deriving (Eq, Show)
data Variable             = NamedVariable String
                          | ArrayAccess (Commented Variable) (Commented Expression)
                          deriving (Eq, Show)
data Expression           = VariableExpression (Commented Variable)
                          | IntLiteral IntString
                          | Parenthesized (Commented Expression)
                          | BinaryExpression (Commented Op) (Commented Expression) (Commented Expression)
                          | Negative (Commented Expression)
                          | Positive (Commented Expression)
                          deriving (Eq, Show)
