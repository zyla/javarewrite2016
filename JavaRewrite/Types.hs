module JavaRewrite.Types where

data RewriteError =
  ConstantFoldingFailed String -- detail message
  deriving (Show, Eq)
