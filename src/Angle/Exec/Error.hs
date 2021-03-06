{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Angle.Exec.Error
Description : Defines main error system used in Angle.
Copyright   : Copyright (C) 2015 Ben Moon
License     : GNU GPL, version 3
Maintainer  : GuiltyDolphin@gmail.com
Stability   : alpha

Defines functions for throwing, catching and handling errors that
occur in Angle.
-}
module Angle.Exec.Error
    (
    -- ** Type errors
      typeAnnWrongErr
    , typeConstrWrongReturnErr
    , typeExpectConstrErr
    , typeMismatchOpErr
    , typeNotValidErr
    , typeUnexpectedErr
    , typeCastErr

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


    -- ** Value errors
    , badRangeErr
    , indexOutOfBoundsErr
    , infiniteRangeErr
    , nonEnumErr
    , divideByZeroErr

    -- ** Control-flow
    , catchBreak
    , catchContinue
    , catchReturn
    , throwBreak
    , throwContinue
    , throwReturn

    -- ** User errors
    , userErr
    , errToKeyword
    , genErrKeyword

    -- ** IO errors
    , alreadyExistsErr
    , doesNotExistErr
    , alreadyInUseErr
    , deviceFullErr
    , eofErr
    , illegalOperationErr
    , permissionErr

    -- ** Syntax errors
    , syntaxErr
    , readErr

    -- ** Include errors
    , noSuchFileErr
    , badSyntaxErr

    -- ** Classes, base types and basic functions
    , AngleError
    , CanError(..)
    , CanErrorWithPos(..)
    , ExecError
    , throwError
    , throwImplementationErr
    , throwExecError
    ) where


import Control.Monad.Except
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified System.IO.Error as IO

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
    getErrorFile :: m (Maybe FilePath)
    -- FIXME: This should maybe just have the line numbers?
    -- Then could perhaps not need the separate position.
    getErrorCallStack :: m [(LangIdent, Stmt)]
    -- getErrorStmt :: m Stmt
    getErrorCall :: m (LangIdent, Stmt)


-- | Errors that can be caught by the user via the try...catch
-- construct.
class KWError e where
    errToKeyword :: e -> LangIdent
    genErrKeyword :: e -> LangIdent
    -- ^ General keyword that can be used to handle multiple exceptions
    -- of this type.


-- | General error structure.
data AngleError = ExecError
    { execErrSourceRef :: SourceRef
    , execErrErr :: ExecError
    , execErrSourceText :: String
    , execErrFile :: Maybe FilePath
    , execErrStack :: [(LangIdent, Stmt)]
    , execErrCall :: (LangIdent, Stmt)
    }
                | ImplementationError String
    deriving (Eq)


implementationErr :: String -> AngleError
implementationErr = ImplementationError


instance Show AngleError where
    show (ImplementationError x) = "Implementation error: " ++ x
    show (ExecError { execErrErr=ee
                      , execErrSourceRef=SourceRef (start,_)
                      , execErrSourceText=es
                      , execErrFile=ef
                      , execErrStack=eStack
                      , execErrCall=eCall
                      })
        = cEf ++ cEStack ++ cECall ++ cEk ++ cEe
          where
            cEf = case ef of
                      Nothing -> ""
                      Just f -> "In file: " ++ f ++ "\n"
            cECall = showCall eCall
            cEStack = concatMap showCall (reverse eStack)
            cEk = concat ["\n(:", showSyn $ errToKeyword ee, ")\n"]
            cEp = show start ++ "\n"
            cEe = show ee
            showCall (c,s) = r
              where
                r = "  line " ++ ln ++ ", in " ++ f ++ "\n" ++ t
                t = "    " ++ st ++ "\n"
                ln = show (stmtLine s + 1)
                f = showSyn c
                st = let (SingleStmt s' _) = s; tos = showSyn s' in if last tos == '\n' then init tos else tos
            stmtLine (SingleStmt _ (SourceRef (l,_))) = lineNo l


instance KWError AngleError where
    errToKeyword (ImplementationError x) = error $ "(Attempt to handle) Implementation error: " ++ show x
    errToKeyword (ExecError { execErrErr = e }) = errToKeyword e

    genErrKeyword (ImplementationError x) = error $ "(Attempt to handle) Implementation error: " ++ show x
    genErrKeyword (ExecError { execErrErr = e }) = genErrKeyword e


-- | Base for errors that occur during execution of code.
data ExecError = TypeError TypeError
                 | NameError NameError
                 | CallError CallError
                 | ValueError ValueError
                 | KeywordError KeywordError
                 | EIOError EIOError
                 | UserError LangIdent
                 | SynError SynError
                 | IncludeError IncludeError
                 | ControlException ControlException
                 deriving (Eq)


-- | Expression produced an invalid type.
typeErr :: TypeError -> ExecError
typeErr    = TypeError


-- | Issue with identifier.
nameErr :: NameError -> ExecError
nameErr    = NameError


-- | Bad function call.
callErr :: CallError -> ExecError
callErr    = CallError


-- | Invalid literal.
valueErr :: ValueError -> ExecError
valueErr     = ValueError


-- | Misused keyword.
keywordErr :: KeywordError -> ExecError
keywordErr = KeywordError


-- | IO Error.
eioErr :: EIOError -> ExecError
eioErr = EIOError


-- | Custom exceptions.
userErr :: LangIdent -> ExecError
userErr = UserError


-- | Syntax errors.
synErr :: SynError -> ExecError
synErr = SynError


-- | Control flow
controlException :: ControlException -> ExecError
controlException = ControlException


-- | Including files
includeErr :: IncludeError -> ExecError
includeErr = IncludeError


instance Show ExecError where
    show (TypeError e)    = "wrong type in expression: " ++ show e
    show (NameError v)    = "name error: " ++ show v
    show (CallError x)    = "call error: " ++ show x
    show (ValueError x) = "value error: " ++ show x
    show (EIOError e) = "io error: " ++ show e
    show (UserError (LangIdent x)) = "user error: " ++ x
    show (KeywordError x) = "keyword error: " ++ show x
    show (SynError e) = "syntax error:\n" ++ show e
    show (ControlException e) = "control: " ++ show e
    show (IncludeError e) = "include: " ++ show e


instance KWError ExecError where
    errToKeyword (TypeError e) = errToKeyword e
    errToKeyword (NameError e) = errToKeyword e
    errToKeyword (CallError e) = errToKeyword e
    errToKeyword (ValueError e) = errToKeyword e
    errToKeyword (EIOError e) = errToKeyword e
    errToKeyword (UserError e) = e
    errToKeyword (KeywordError e) = errToKeyword e
    errToKeyword (SynError e) = errToKeyword e
    errToKeyword (ControlException e) = errToKeyword e
    errToKeyword (IncludeError e) = errToKeyword e

    genErrKeyword (TypeError e) = genErrKeyword e
    genErrKeyword (NameError e) = genErrKeyword e
    genErrKeyword (CallError e) = genErrKeyword e
    genErrKeyword (ValueError e) = genErrKeyword e
    genErrKeyword (EIOError e) = genErrKeyword e
    genErrKeyword (UserError{}) = LangIdent "user"
    genErrKeyword (KeywordError e) = genErrKeyword e
    genErrKeyword (SynError e) = genErrKeyword e
    genErrKeyword (ControlException e) = genErrKeyword e
    genErrKeyword (IncludeError e) = genErrKeyword e


-- | Errors involving types.
data TypeError = TypeMismatch   LangType LangType
               | TypeUnexpected LangType LangType
               | TypeNotValid   LangType
               | TypeCast LangType LangType
               | TypeMismatchOp LangType LangType
               | TypeExpectConstr LangLit LangIdent
               | TypeConstrWrongReturn LangIdent LangType
               | TypeAnnWrong AnnType AnnType
               deriving (Eq)


-- | Wrong type has been passed and required type is known.
typeUnexpectedErr :: LangType -> LangType -> ExecError
typeUnexpectedErr t1 = typeErr . TypeUnexpected t1


-- | Wrong type has been passed and required type is not known.
typeNotValidErr :: LangLit -> ExecError
typeNotValidErr     = typeErr . TypeNotValid . typeOf


-- | Expecting types to be the same.
typeMismatchOpErr :: LangLit -> LangLit -> ExecError
typeMismatchOpErr x y = typeErr $ (TypeMismatchOp `on` typeOf) x y


-- | Expecting value to satisfy given parameter constraint.
typeExpectConstrErr :: LangLit -> LangIdent -> ExecError
typeExpectConstrErr cls = typeErr . TypeExpectConstr cls


-- | Function used as parameter constraint did not return boolean
-- value.
typeConstrWrongReturnErr :: LangIdent -> LangType -> ExecError
typeConstrWrongReturnErr cls = typeErr . TypeConstrWrongReturn cls


-- | Value did not satisfy given annotation constraint.
typeAnnWrongErr :: AnnType -> AnnType -> ExecError
typeAnnWrongErr e = typeErr . TypeAnnWrong e


-- | Cannot coerce one type to another
typeCastErr :: LangType -> LangType -> ExecError
typeCastErr t1 = typeErr . TypeCast t1


instance Show TypeError where
    show (TypeMismatch l r)   = "type mismatch: got (" ++ show l ++ ", " ++ show r ++ ") but both types should be the same"
    show (TypeUnexpected l r) = "unexpected type: " ++ show l ++ ", expecting: " ++ show r
    show (TypeNotValid l)     = "type not valid : " ++ show l
    show (TypeCast l r) = "cannot convert " ++ show l ++ " to " ++ show r
    show (TypeMismatchOp l r) = "cannot perform operation on types " ++ show l ++ " and " ++ show r
    show (TypeExpectConstr v c) = "expecting value that satisfies function '" ++ showSyn c ++ "' but got: " ++ showSyn v
    show (TypeConstrWrongReturn c t) = "bad class: " ++ showSyn c ++ ", expecting return value of type bool, but got " ++ show t
    show (TypeAnnWrong t1 t2) = "bad type in function call, expecting " ++ show t1 ++ " but got " ++ show t2


instance KWError TypeError where
    errToKeyword (TypeMismatch{}) = LangIdent "typeMismatch"
    errToKeyword (TypeUnexpected{}) = LangIdent "typeUnexpected"
    errToKeyword (TypeNotValid{}) = LangIdent "typeNotValid"
    errToKeyword (TypeCast{}) = LangIdent "typeCast"
    errToKeyword (TypeMismatchOp{}) = LangIdent "typeMismatchOp"
    errToKeyword (TypeExpectConstr{}) = LangIdent "typeExpectConstr"
    errToKeyword (TypeConstrWrongReturn{}) = LangIdent "typeConstrWrongReturn"
    errToKeyword (TypeAnnWrong{}) = LangIdent "typeAnnWrong"
    genErrKeyword _ = LangIdent "typeError"


-- | Errors involving identifiers and names.
data NameError = NameNotDefined LangIdent
               | NameNotDefinedFun LangIdent
               | NameNotDefinedLit LangIdent
               | NameNotOp LangIdent
               | AssignToBuiltin LangIdent (Maybe String)
               deriving (Eq)


-- | Given identifier has no definition.
nameNotDefinedErr :: LangIdent -> ExecError
nameNotDefinedErr  = nameErr . NameNotDefined


-- | Given identifier has no lambda assigned.
nameNotDefinedFunErr :: LangIdent -> ExecError
nameNotDefinedFunErr = nameErr . NameNotDefinedFun


-- | Given identifier has no value assigned.
nameNotDefinedLitErr :: LangIdent -> ExecError
nameNotDefinedLitErr = nameErr . NameNotDefinedLit


-- | Attempt to re-assign a builtin variable.
assignToBuiltinErr :: LangIdent -> Maybe String -> ExecError
assignToBuiltinErr name = nameErr . AssignToBuiltin name


instance Show NameError where
    show (NameNotDefined  (LangIdent name)) = "not in scope: "         ++ name
    show (NameNotDefinedFun (LangIdent name)) = "not a valid function: " ++ name
    show (NameNotDefinedLit (LangIdent name)) = "no value assigned: "    ++ name
    show (NameNotOp       (LangIdent name)) = "not a valid operator: " ++ name
    show (AssignToBuiltin (LangIdent name) reason) = "cannot assign to builtin: " ++ name
                                                       ++ maybe "" ("\n" ++) reason


instance KWError NameError where
    errToKeyword (NameNotDefined{}) = LangIdent "nameNotDefined"
    errToKeyword (NameNotDefinedFun{}) = LangIdent "nameNotDefinedFun"
    errToKeyword (NameNotDefinedLit{}) = LangIdent "nameNotDefinedLit"
    errToKeyword (NameNotOp{}) = LangIdent "nameNotOp"
    errToKeyword (AssignToBuiltin{}) = LangIdent "assignToBuiltin"
    genErrKeyword _ = LangIdent "nameError"


-- | Errors involving operator and function calls.
data CallError = WrongNumberOfArguments Int Int
               | BuiltIn String
               | MalformedSignature String
    deriving (Eq)


-- | Attempted to pass an invalid number of arguments to a function.
wrongNumberOfArgumentsErr :: Int -> Int -> ExecError
wrongNumberOfArgumentsErr expect = callErr . WrongNumberOfArguments expect


-- | Error when calling builtin.
callBuiltinErr :: String -> ExecError
callBuiltinErr = callErr . BuiltIn


-- | Order of arguments not valid in scenario.
malformedSignatureErr :: String -> ExecError
malformedSignatureErr = callErr . MalformedSignature


instance Show CallError where
    show (WrongNumberOfArguments x y) = "wrong number of arguments: expected " ++ show x ++ " but got " ++ show y
    show (BuiltIn x) = "builtin: " ++ x
    show (MalformedSignature x) = "malformed signature: " ++ x


instance KWError CallError where
    errToKeyword (WrongNumberOfArguments{}) = LangIdent "wrongNumberOfArguments"
    errToKeyword (BuiltIn{}) = LangIdent "builtin"
    errToKeyword (MalformedSignature{}) = LangIdent "malformedSignature"
    genErrKeyword _ = LangIdent "callError"


-- | Errors involving keywords.
data KeywordError = ReturnFromGlobal
                    deriving (Eq)


-- | Return keyword used in wrong place.
returnFromGlobalErr :: ExecError
returnFromGlobalErr = keywordErr ReturnFromGlobal


instance Show KeywordError where
    show ReturnFromGlobal = "return from outermost scope"


instance KWError KeywordError where
    errToKeyword ReturnFromGlobal = LangIdent "returnFromGlobal"
    genErrKeyword _ = LangIdent "keywordError"


-- | Raise a 'ExecError' into an 'AngleError'.
throwExecError :: (CanErrorWithPos m, Monad m) => ExecError -> m a
throwExecError e = do
  errPosRef <- getErrorPos
  errSource <- getErrorSource
  errPath <- getErrorFile
  errStack <- getErrorCallStack
  errCall <- getErrorCall
  throwAE ExecError { execErrSourceRef = errPosRef
                         , execErrSourceText = errSource
                         , execErrErr = e
                         , execErrFile = errPath
                         , execErrStack = errStack
                         , execErrCall = errCall
                         }


-- | Throw a fatal error caused by the internals of the implementation.
throwImplementationErr :: (CanError m) => String -> m a
throwImplementationErr = throwAE . implementationErr


-- | Errors involving literals.
data ValueError = IndexOutOfBoundsError Int
              | BadRange LangType (Maybe LangType) (Maybe LangType)
              | InfiniteRange
              | NonEnum LangType
              | DivideByZero
              deriving (Eq)


-- | Attempt to access a non-existant index of a list.
indexOutOfBoundsErr :: Int -> ExecError
indexOutOfBoundsErr = valueErr . IndexOutOfBoundsError


-- | Types are not uniform in range.
badRangeErr :: LangType -> Maybe LangType -> Maybe LangType -> ExecError
badRangeErr t1 t2 = valueErr . BadRange t1 t2


-- | Range is infinite.
infiniteRangeErr :: ExecError
infiniteRangeErr = valueErr InfiniteRange


-- | Non-enumerable type used where enumerable expected.
nonEnumErr :: LangType -> ExecError
nonEnumErr = valueErr . NonEnum


-- | Attempted division by zero.
divideByZeroErr :: ExecError
divideByZeroErr = valueErr DivideByZero


instance Show ValueError where
    show (IndexOutOfBoundsError x) = "index out of bounds: " ++ show x
    show (BadRange t1 t2 t3) = "bad range: all types should be same, but got: " ++ show t1 ++ concatMap ((", "++) . show) (catMaybes [t2,t3])
    show InfiniteRange = "infinite range"
    show (NonEnum t) = "expected enumerable type but got: " ++ show t
    show DivideByZero = "attempted division by zero"


instance KWError ValueError where
    errToKeyword (IndexOutOfBoundsError{}) = LangIdent "indexOutOfBounds"
    errToKeyword (BadRange{}) = LangIdent "badRange"
    errToKeyword InfiniteRange = LangIdent "infiniteRange"
    errToKeyword (NonEnum{}) = LangIdent "nonEnum"
    errToKeyword DivideByZero = LangIdent "divideByZero"
    genErrKeyword _ = LangIdent "valueError"



-- | Errors that occur during IO operations.
data EIOError = AlreadyExists IOError
              | DoesNotExist IOError
              | AlreadyInUse IOError
              | DeviceFull IOError
              | EOF IOError
              | IllegalOperation IOError
              | Permission IOError
              deriving (Eq)


alreadyExistsErr :: IOError -> ExecError
alreadyExistsErr = eioErr . AlreadyExists


doesNotExistErr :: IOError -> ExecError
doesNotExistErr = eioErr . DoesNotExist


alreadyInUseErr :: IOError -> ExecError
alreadyInUseErr = eioErr . AlreadyInUse


deviceFullErr :: IOError -> ExecError
deviceFullErr = eioErr . DeviceFull


eofErr :: IOError -> ExecError
eofErr = eioErr . EOF


illegalOperationErr :: IOError -> ExecError
illegalOperationErr = eioErr . IllegalOperation


permissionErr :: IOError -> ExecError
permissionErr = eioErr . Permission


eioErrorShow :: IOError -> String
eioErrorShow e = l ++ fn ++ s
  where
    fn = case IO.ioeGetFileName e of
                Nothing -> ""
                Just n -> show n ++ ": "
    l = case IO.ioeGetLocation e of
           "openFile" -> "open file: "
           "readFile" -> "read file: "
           "readProcess: runInteractiveProcess: exec" -> "shell: "
           x -> x ++ ": "
    s = IO.ioeGetErrorString e


instance Show EIOError where
    show (AlreadyExists e) = eioErrorShow e
    show (DoesNotExist e) = eioErrorShow e
    show (AlreadyInUse e) = eioErrorShow e
    show (DeviceFull e) = eioErrorShow e
    show (EOF e) = eioErrorShow e
    show (IllegalOperation e) = eioErrorShow e
    show (Permission e) = eioErrorShow e


instance KWError EIOError where
    errToKeyword (AlreadyExists{}) = LangIdent "alreadyExists"
    errToKeyword (DoesNotExist{}) = LangIdent "doesNotExist"
    errToKeyword (AlreadyInUse{}) = LangIdent "alreadyInUse"
    errToKeyword (DeviceFull{}) = LangIdent "deviceFull"
    errToKeyword (EOF{}) = LangIdent "eof"
    errToKeyword (IllegalOperation{}) = LangIdent "illegalOperation"
    errToKeyword (Permission{}) = LangIdent "permission"

    genErrKeyword _ = LangIdent "ioError"


-- | Represents errors that occur during run-time parsing.
data SynError = SyntaxError ParseError
              | ReadError String
              deriving (Eq)


-- | Error when parsing text intended to be code.
syntaxErr :: ParseError -> ExecError
syntaxErr = synErr . SyntaxError


-- | Error when parsing non-code.
readErr :: String -> ExecError
readErr = synErr . ReadError


instance Show SynError where
    show (SyntaxError e) = show e
    show (ReadError e) = "could not read: " ++ show e


instance KWError SynError where
    errToKeyword (SyntaxError{}) = LangIdent "syntax"
    errToKeyword (ReadError{}) = LangIdent "read"

    genErrKeyword _ = LangIdent "syntaxError"



data IncludeError = NoSuchFile FilePath
                  | BadSyntax ParseError
                  deriving (Eq)


noSuchFileErr :: FilePath -> ExecError
noSuchFileErr = includeErr . NoSuchFile


badSyntaxErr :: ParseError -> ExecError
badSyntaxErr = includeErr . BadSyntax


instance Show IncludeError where
    show (NoSuchFile fp) = "no such file: " ++ fp
    show (BadSyntax e) = show e


instance KWError IncludeError where
    errToKeyword (NoSuchFile{}) = LangIdent "doesNotExist"
    errToKeyword (BadSyntax{}) = LangIdent "syntax"
    genErrKeyword _ = LangIdent "include"


-- | Used for control-flow within the language.
--
-- These should never make it to the user, but instead be caught in
-- internal code.
data ControlException = ControlReturn LangLit
                      | ControlBreak (Maybe LangLit)
                      | ControlContinue
                      deriving (Eq)


controlReturn :: LangLit -> ExecError
controlReturn = controlException . ControlReturn


controlBreak :: Maybe LangLit -> ExecError
controlBreak = controlException . ControlBreak


controlContinue :: ExecError
controlContinue = controlException ControlContinue


instance Show ControlException where
    show (ControlReturn{}) = "return made it to top level"
    show (ControlBreak{}) = "break outside of loop"
    show ControlContinue = "continue outside of loop"


instance KWError ControlException where
    errToKeyword (ControlReturn{}) = LangIdent "return"
    errToKeyword (ControlBreak{}) = LangIdent "break"
    errToKeyword ControlContinue = LangIdent "continue"

    genErrKeyword _ = LangIdent "controlException"


-- | Used for the 'return' keyword.
throwReturn :: (CanErrorWithPos m) => LangLit -> m a
throwReturn = throwExecError . controlReturn -- throwAE . controlReturn


-- | Used for the 'break' keyword.
throwBreak :: (CanErrorWithPos m) => Maybe LangLit -> m a
throwBreak = throwExecError . controlBreak -- throwAE . controlBreak


-- | Used for the 'continue' keyword.
throwContinue :: (CanErrorWithPos m) => m a
throwContinue = throwExecError controlContinue -- throwAE controlContinue


-- | Catch 'ControlReturn', but allow other errors to propagate.
catchReturn :: (CanError m) => m a -> (LangLit -> m a) -> m a
catchReturn ex h = ex `catchAE`
                   (\e -> case e of
                            ExecError { execErrErr = ControlException (ControlReturn v) } -> h v
                            err -> throwAE err)


-- | Catch 'ControlBreak', but allow other errors to propagate.
catchBreak :: (CanError m) => m a -> (Maybe LangLit -> m a) -> m a
catchBreak ex h
    = ex `catchAE`
        (\e -> case e of
            ExecError
              { execErrErr = ControlException (ControlBreak v)
              } -> h v
            err -> throwAE err)


-- | Catch 'ControlContinue', but allow other errors to propagate.
catchContinue :: (CanError m) => m a -> m a -> m a
catchContinue ex v
    = ex `catchAE`
        (\e -> case e of
            ExecError
              { execErrErr = ControlException ControlContinue } -> v
            err -> throwAE err)
