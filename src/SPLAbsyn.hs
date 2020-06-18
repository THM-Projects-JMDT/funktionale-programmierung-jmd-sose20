module SPLAbsyn where

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SPL-AST structure - extended with comments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}


data Program              = Program [GlobalDeclaration]
                          deriving Show 
data GlobalDeclaration    = TypeDeclaration String TypeExpression [Comment]
                          | ProcedureDeclaration String [ParameterDeclaration] [VariableDeclaration] [Statement] [Comment]
                          | GlobalComment Comment
                          deriving Show
data TypeExpression       = ArrayTypeExpression Int TypeExpression
                          | NamedTypeExpression String
                          deriving Show
data ParameterDeclaration = ParameterDeclaration String TypeExpression Bool 
                          deriving Show
data VariableDeclaration  = VariableDeclaration String TypeExpression [Comment]
                          deriving Show
data Statement            = AssignStatement Variable Expression [Comment]
                          | CallStatement String [Expression] [Comment]
                          | CompoundStatement [Statement] [Comment]
                          | EmptyStatement [Comment]
                          | IfStatement Expression Statement Statement [Comment]
                          | WhileStatement Expression Statement [Comment]
                          | StatementComment Comment
                          deriving Show
data Variable             = NamedVariable String
                          | ArrayAccess Variable Expression
                          deriving Show
data Expression           = VariableExpression Variable
                          | IntLiteral Int
                          | BinaryExpression Op Expression Expression
                          deriving Show

type Comment = String

data Op = Lt | Ne | Asgn | Plus | Slash | Star | Gt | Le | Minus | Ge | Eq 
          deriving (Eq, Show) 