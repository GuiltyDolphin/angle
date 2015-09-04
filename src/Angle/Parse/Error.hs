{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Angle.Parse.Error
Description : Defines main error system used in Angle.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines functions for throwing, catching and handling errors that
occur in Angle.
-}
module Angle.Parse.Error
    (
    -- ** Type errors
      typeAnnWrongErr
    , typeConstrWrongReturnErr
    , typeExpectConstrErr
    , typeMismatchOpErr
    , typeNotValidErr
    , typeUnexpectedErr

    -- ** Name errors
    , assignToBuiltinErr
    , nameNotDefinedErr
    , nameNotDefinedFunErr
    , nameNotDefinedLitErr

    -- ** Call errors
    , callBuiltinErr
    , malformedSignatureErr
    , wrongNumberOfArgumentsErr


    -- ** Keyword errors
    , returnFromGlobalErr


    -- ** Literal errors
    , badRangeErr
    , indexOutOfBoundsErr

    -- ** Control-flow
    , catchBreak
    , catchContinue
    , catchReturn
    , throwBreak
    , throwContinue
    , throwReturn

    -- ** Classes, base types and basic functions
    , AngleError
    , CanError(..)
    , CanErrorWithPos(..)
    , ParserError
    , throwError
    , throwImplementationErr
    , throwParserError
    ) where


import Control.Monad.Except
import Data.Function (on)
import Data.Maybe (catMaybes)

import Angle.Scanner
import Angle.Types.Lang


-- | Instances are types that can throw Angle errors.
class (Monad m) => CanError (m :: * -> *) where
    throwAE :: AngleError -> m a
    catchAE :: m a -> (AngleError -> m a) -> m a


instance CanError (Either AngleError) where
    throwAE = Left
    catchAE e f = case e of
                    r@(Right _) -> r
                    Left l -> f l


-- | Instances are types that can throw Angle errors and provide
-- information about the position at which the error occurred.
class (CanError m) => CanErrorWithPos m where
    getErrorPos :: m SourceRef
    getErrorSource :: m String


-- | General error structure.
data AngleError = ParserError
    { parserErrSourceRef :: SourceRef
    , parserErrErr :: ParserError
    , parserErrSourceText :: String
    }
                | ImplementationError String
                | ControlException ControlException


implementationErr :: String -> AngleError
implementationErr = ImplementationError


controlException :: ControlException -> AngleError
controlException = ControlException


instance Show AngleError where
    show (ImplementationError x) = "Implementation error: " ++ x
    show (ParserError { parserErrErr=ee
                      , parserErrSourceRef=SourceRef (start,_)
                      , parserErrSourceText=es
                      })
        = cEp ++ cEt ++ cEe
          where
            cEp = show start ++ "\n"
            cEt = let lns = lines es in
                      if null lns
                      then "no source\n"
                      else replicate (colNo start) ' '
                               ++ "v\n"
                               ++ lns !! lineNo start
                               ++ "\n"
            cEe = show ee
    show (ControlException _) =
        error "show: control exception made it to show"


-- | Base for errors that occur during execution of code.
data ParserError = TypeError TypeError
                 | NameError NameError
                 | CallError CallError
                 | DefaultError String
                 | LitError LitError
                 | KeywordError KeywordError
                 | UserError String


-- | Expression produced an invalid type.
typeErr :: TypeError -> ParserError
typeErr    = TypeError


-- | Issue with identifier.
nameErr :: NameError -> ParserError
nameErr    = NameError


-- | Bad function call.
callErr :: CallError -> ParserError
callErr    = CallError


-- | Invalid literal.
litErr :: LitError -> ParserError
litErr     = LitError


-- | Misused keyword.
keywordErr :: KeywordError -> ParserError
keywordErr = KeywordError


instance Show ParserError where
    show (TypeError e)    = "wrong type in expression: " ++ show e
    show (NameError v)    = "name error: " ++ show v
    show (CallError x)    = "call error: " ++ show x
    show (DefaultError s) = "defaultError: " ++ s
    show (LitError x) = "literal error: " ++ show x
    show (UserError x) = "user error: " ++ x
    show (KeywordError x) = "keyword error: " ++ show x


-- | Errors involving types.
data TypeError = TypeMismatch   LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid   LangType
               | TypeCast LangType LangType
               | TypeMismatchOp LangType LangType
               | TypeExpectConstr LangLit LangIdent
               | TypeConstrWrongReturn LangIdent LangType
               | TypeAnnWrong AnnType AnnType


-- | Wrong type has been passed and required type is known.
typeUnexpectedErr :: LangType -> LangType -> ParserError
typeUnexpectedErr t1 = typeErr . TypeUnexpected t1


-- | Wrong type has been passed and required type is not known.
typeNotValidErr :: LangLit -> ParserError
typeNotValidErr     = typeErr . TypeNotValid . typeOf


-- | Expecting types to be the same.
typeMismatchOpErr :: LangLit -> LangLit -> ParserError
typeMismatchOpErr x y = typeErr $ (TypeMismatchOp `on` typeOf) x y


-- | Expecting value to satisfy given parameter constraint.
typeExpectConstrErr :: LangLit -> LangIdent -> ParserError
typeExpectConstrErr cls = typeErr . TypeExpectConstr cls


-- | Function used as parameter constraint did not return boolean
-- value.
typeConstrWrongReturnErr :: LangIdent -> LangType -> ParserError
typeConstrWrongReturnErr cls = typeErr . TypeConstrWrongReturn cls


-- | Value did not satisfy given annotation constraint.
typeAnnWrongErr :: AnnType -> AnnType -> ParserError
typeAnnWrongErr e = typeErr . TypeAnnWrong e


instance Show TypeError where
    show (TypeMismatch l r)   = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l)     = "type not valid for scenario: " ++ show l
    show (TypeCast l r) = "cannot convert " ++ show l ++ " to " ++ show r
    show (TypeMismatchOp l r) = "cannot perform operation on types " ++ show l ++ " and " ++ show r
    show (TypeExpectConstr v c) = "expecting value that satisfies function '" ++ showSyn c ++ "' but got: " ++ showSyn v
    show (TypeConstrWrongReturn c t) = "bad class: " ++ showSyn c ++ ", expecting return value of type bool, but got " ++ show t
    show (TypeAnnWrong t1 t2) = "bad type in function call, expecting " ++ show t1 ++ " but got " ++ show t2


-- | Errors involving identifiers and names.
data NameError = NameNotDefined LangIdent
               | NameNotDefinedFun LangIdent
               | NameNotDefinedLit LangIdent
               | NameNotOp LangIdent
               | AssignToBuiltin LangIdent (Maybe String)


-- | Given identifier has no definition.
nameNotDefinedErr :: LangIdent -> ParserError
nameNotDefinedErr  = nameErr . NameNotDefined


-- | Given identifier has no lambda assigned.
nameNotDefinedFunErr :: LangIdent -> ParserError
nameNotDefinedFunErr = nameErr . NameNotDefinedFun


-- | Given identifier has no value assigned.
nameNotDefinedLitErr :: LangIdent -> ParserError
nameNotDefinedLitErr = nameErr . NameNotDefinedLit


-- | Attempt to re-assign a builtin variable.
assignToBuiltinErr :: LangIdent -> Maybe String -> ParserError
assignToBuiltinErr name = nameErr . AssignToBuiltin name


instance Show NameError where
    show (NameNotDefined  (LangIdent name)) = "not in scope: "         ++ name
    show (NameNotDefinedFun (LangIdent name)) = "not a valid function: " ++ name
    show (NameNotDefinedLit (LangIdent name)) = "no value assigned: "    ++ name
    show (NameNotOp       (LangIdent name)) = "not a valid operator: " ++ name
    show (AssignToBuiltin (LangIdent name) reason) = "cannot assign to builtin: " ++ name
                                                       ++ maybe "" ("\n" ++) reason


-- | Errors involving operator and function calls.
data CallError = WrongNumberOfArguments Int Int
               | BuiltIn String
               | MalformedSignature String
    deriving (Eq)


-- | Attempted to pass an invalid number of arguments to a function.
wrongNumberOfArgumentsErr :: Int -> Int -> ParserError
wrongNumberOfArgumentsErr expect = callErr . WrongNumberOfArguments expect


-- | Error when calling builtin.
callBuiltinErr :: String -> ParserError
callBuiltinErr = callErr . BuiltIn


-- | Order of arguments not valid in scenario.
malformedSignatureErr :: String -> ParserError
malformedSignatureErr = callErr . MalformedSignature


instance Show CallError where
    show (WrongNumberOfArguments x y) = "wrong number of arguments: expected " ++ show x ++ " but got " ++ show y
    show (BuiltIn x) = "builtin: " ++ x
    show (MalformedSignature x) = "malformed signature: " ++ x


-- | Errors involving keywords.
data KeywordError = ReturnFromGlobal
                    deriving (Eq)


-- | Return keyword used in wrong place.
returnFromGlobalErr :: ParserError
returnFromGlobalErr = keywordErr ReturnFromGlobal


instance Show KeywordError where
    show ReturnFromGlobal = "return from outermost scope"


-- | Raise a 'ParserError' into an 'AngleError'.
throwParserError :: (CanErrorWithPos m, Monad m) => ParserError -> m a
throwParserError e = do
  errPosRef <- getErrorPos
  errSource <- getErrorSource
  throwAE ParserError { parserErrSourceRef = errPosRef
                         , parserErrSourceText = errSource
                         , parserErrErr = e
                         }


-- | Throw a fatal error caused by the internals of the implementation.
throwImplementationErr :: (CanError m) => String -> m a
throwImplementationErr = throwAE . implementationErr


-- | Errors involving literals.
data LitError = IndexOutOfBoundsError Int
              | BadRange LangType (Maybe LangType) (Maybe LangType)
              deriving (Eq)


-- | Attempt to access a non-existant index of a list.
indexOutOfBoundsErr :: Int -> ParserError
indexOutOfBoundsErr = litErr . IndexOutOfBoundsError


-- | Types are not uniform in range.
badRangeErr :: LangType -> Maybe LangType -> Maybe LangType -> ParserError
badRangeErr t1 t2 = litErr . BadRange t1 t2


instance Show LitError where
    show (IndexOutOfBoundsError x) = "index out of bounds: " ++ show x
    show (BadRange t1 t2 t3) = "bad range: all types should be same, but got: " ++ show t1 ++ concatMap ((", "++) . show) (catMaybes [t2,t3])


-- | Used for control-flow within the language.
--
-- These should never make it to the user, but instead be caught in
-- internal code.
data ControlException = ControlReturn LangLit
                      | ControlBreak (Maybe LangLit)
                      | ControlContinue
                      deriving (Show, Eq)


controlReturn :: LangLit -> AngleError
controlReturn = controlException . ControlReturn


controlBreak :: Maybe LangLit -> AngleError
controlBreak = controlException . ControlBreak


controlContinue :: AngleError
controlContinue = controlException ControlContinue


-- | Used for the 'return' keyword.
throwReturn :: (CanError m) => LangLit -> m a
throwReturn = throwAE . controlReturn


-- | Used for the 'break' keyword.
throwBreak :: (CanError m) => Maybe LangLit -> m a
throwBreak = throwAE . controlBreak


-- | Used for the 'continue' keyword.
throwContinue :: (CanError m) => m a
throwContinue = throwAE controlContinue


-- | Catch 'ControlReturn', but allow other errors to propagate.
catchReturn :: (CanError m) => m a -> (LangLit -> m a) -> m a
catchReturn ex h = ex `catchAE`
                   (\e -> case e of
                            ControlException (ControlReturn v) -> h v
                            err -> throwAE err)


-- | Catch 'ControlBreak', but allow other errors to propagate.
catchBreak :: (CanError m) => m a -> (Maybe LangLit -> m a) -> m a
catchBreak ex h = ex `catchAE`
                  (\e -> case e of
                           ControlException (ControlBreak v) -> h v
                           err -> throwAE err)


-- | Catch 'ControlContinue', but allow other errors to propagate.
catchContinue :: (CanError m) => m a -> m a -> m a
catchContinue ex v = ex `catchAE`
                     (\e -> case e of
                              ControlException ControlContinue -> v
                              err -> throwAE err)
