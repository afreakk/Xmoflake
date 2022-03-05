module Calculator (calculatorPrompt) where
import           XMonad.Prompt (XPrompt, showXPrompt, completionToCommand, XPConfig, mkXPrompt)
import           XMonad.Util.Run (runProcessWithInput)
import XMonad
import Data.Maybe
import Utils
import Control.Monad

data CalculatorMode = CalculatorMode

instance XPrompt CalculatorMode where
    showXPrompt CalculatorMode     = "calc> "
    completionToCommand _ = id

maybeValueToClipboard :: Show a => Maybe a -> X ()
maybeValueToClipboard (Just v) = spawn ("echo -n " ++ show v ++ " | "++stdinToClip++" &> /dev/null")
maybeValueToClipboard Nothing  = return ()

calculatorPrompt :: XPConfig -> X ()
calculatorPrompt c = mkXPrompt CalculatorMode c doCalc calcToClipboard

calcToClipboard :: [Char] -> X ()
calcToClipboard = doCalc >=> (maybeValueToClipboard . listToMaybe)

doCalc :: MonadIO m => [Char] -> m [String]
doCalc [] = return []
doCalc s = fmap (lines . stripTab) (runProcessWithInput "calc" [s] "")

stripTab :: [a] -> [a]
stripTab (_:xs) = xs
stripTab [] = []
