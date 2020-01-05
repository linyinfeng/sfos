module Main
  ( main
  )
where

import           Sfos.Parser
import           Sfos.Show
import           Sfos.Syntax
import           Sfos.Evaluation
import           Sfos.Typing
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Show.Prettyprint
import           Data.Either
import           Control.Monad
import qualified Options.Applicative           as O
import           Data.Semigroup                 ( (<>) )
import qualified Data.ByteString.UTF8          as UTF8
import           System.IO
import           System.Console.ANSI
import           System.Exit

data Options = Options { optPath :: Maybe String
                       , optVerbose :: Bool
                       , optContinue :: Bool
                       }
  deriving Show

options :: O.Parser Options
options =
  Options
    <$> O.optional
          (  O.strOption
          $  O.long "file"
          <> O.short 'f'
          <> O.metavar "PATH"
          <> O.help "Input file"
          )
    <*> O.switch
          (O.long "verbose" <> O.short 'v' <> O.help "Show verbose output")
    <*> O.switch
          (O.long "continue" <> O.short 'c' <> O.help
            "Continue when typing error"
          )

main :: IO ()
main = main' =<< O.execParser opts
 where
  opts = O.info
    (options O.<**> O.helper)
    (O.fullDesc <> O.progDesc "REPL for sfos" <> O.header
      "sfos - system F omega with subtyping"
    )

main' :: Options -> IO ()
main' opts = do
  file <- case optPath opts of
    Nothing   -> return stdin
    Just path -> openFile path ReadMode
  hSetEncoding file   utf8
  hSetEncoding stdout utf8
  fileContents <- hGetContents file
  let fileDelta = case optPath opts of
        Nothing   -> Columns 0 0
        Just path -> Directed (UTF8.fromString path) 0 0 0 0
  let parser = parseTerm <* eof
  let parsed = parseString parser fileDelta fileContents
  case parsed of
    Success p -> sfosRepl opts p
    Failure e -> print (_errDoc e)

sfosRepl :: Options -> (TypingContext -> Term) -> IO ()
sfosRepl opts term' = do
  let ctx  = []
  let term = term' ctx
  outputTerm opts ctx term
  let ty = typeOf ctx term
  outputTypingResult opts ctx ty
  let evaled = eval term
  outputEvalResult opts ctx evaled
  if isRight evaled then exitSuccess else exitFailure

outputTerm :: Options -> TypingContext -> Term -> IO ()
outputTerm opts ctx t = whenVerbose opts $ do
  withColor Blue $ putStr "term(debug): "
  prettyPrint t
  withColor Green $ putStr "term: "
  print (WithCtx ctx t)

outputTypingResult :: Options -> TypingContext -> Type -> IO ()
outputTypingResult opts ctx ty = whenVerbose opts $ do
  withColor Blue $ putStr "type(debug): "
  prettyPrint ty
  withColor Green $ putStr "type: "
  print (WithCtx ctx ty)

outputEvalResult :: Options -> TypingContext -> Either Term Term -> IO ()
outputEvalResult opts ctx (Left stuckTerm) = do
  whenVerbose opts $ do
    withColor Blue $ putStr "debug: "
    prettyPrint stuckTerm
  withColor Red $ putStr "stucked: "
  print (WithCtx ctx stuckTerm)
outputEvalResult opts ctx (Right term) = do
  whenVerbose opts $ do
    withColor Blue $ putStr "debug: "
    prettyPrint term
  withColor Green (print $ WithCtx ctx term)

whenVerbose :: Monad m => Options -> m () -> m ()
whenVerbose opts = when (optVerbose opts)

withColor :: Color -> IO () -> IO ()
withColor c = withSGR [SetColor Foreground Dull c]

withSGR :: [SGR] -> IO a -> IO a
withSGR sgr x = do
  setSGR sgr
  res <- x
  setSGR [Reset]
  return res
