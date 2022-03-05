module PassFork (
                            -- * Usage
                            -- $usage

                              passClipUsernamePrompt
                            , passClipPasswordPrompt
                            , passClipOTPPrompt
                            , passTypeOTPPrompt
                            , passGeneratePrompt
                            , passRemovePrompt
                            , passEditPrompt
                            , passTypePasswordPrompt
                            , passAppendOTPPrompt
                            , passTypeUsernamePrompt
                            , passAutofillPrompt
                            , passShowPrompt
                            ) where

import XMonad.Core
import XMonad.Prompt ( XPrompt
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , XPConfig
                     , mkXPrompt
                     , searchPredicate)
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension, dropExtension, combine)
import System.Posix.Env (getEnv)
import XMonad.Util.Run (runProcessWithInput, runInTerm)
import Utils (stdinToClip, alacrittyFloatingOpt)

type Predicate = String -> String -> Bool

getPassCompl :: [String] -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default password store folder in $HOME/.password-store
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = fmap passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

-- | A pass prompt factory
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (getPassCompl passwords $ searchPredicate xpconfig) passwordFunction

passClipPasswordPrompt :: XPConfig -> X ()
passClipPasswordPrompt = mkPassPrompt "Select password" clipPassword

passClipUsernamePrompt = mkPassPrompt "Select username" clipUsername

passClipOTPPrompt :: XPConfig -> X ()
passClipOTPPrompt = mkPassPrompt "Select OTP to clip" clipOTP

passTypeOTPPrompt :: XPConfig -> X ()
passTypeOTPPrompt = mkPassPrompt "Select OTP to type" typeOTP

passAppendOTPPrompt :: XPConfig -> X ()
passAppendOTPPrompt = mkPassPrompt "Select where to append OTP secret" appendOTP

passGeneratePrompt :: String -> XPConfig -> X ()
passGeneratePrompt passOpts = mkPassPrompt "Generate password for" (passGenerate passOpts)

passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

passTypePasswordPrompt :: XPConfig -> X ()
passTypePasswordPrompt = mkPassPrompt "Type password" typePassword

passAutofillPrompt :: XPConfig -> X ()
passAutofillPrompt = mkPassPrompt "Type usr & pw" autofill

passTypeUsernamePrompt :: XPConfig -> X ()
passTypeUsernamePrompt = mkPassPrompt "Type username" typeUsername

passEditPrompt :: XPConfig -> X ()
passEditPrompt = mkPassPrompt "Edit password" edit

passShowPrompt :: XPConfig -> X ()
passShowPrompt = mkPassPrompt "Edit password" showAll

showAll :: String -> X ()
showAll passLabel = runInFishTermWithGPG_TTY $ "pass show " ++ escapedPassLabel passLabel ++ " | " ++ showInVimScratchPad

appendOTP :: String -> X ()
appendOTP passLabel = runInFishTermWithGPG_TTY $ "pass otp append " ++ escapedPassLabel passLabel ++ " <(zbarimg (maim -q --select --hidecursor /dev/stdout | psub) --raw -q | psub)"

clipPassword :: String -> X ()
clipPassword passLabel = spawn $ workaroundPass ++ " show " ++ escapedPassLabel passLabel ++ " | " ++ extractPassword ++ " | " ++ stdinToClip

clipUsername :: String -> X ()
clipUsername passLabel = spawn $ workaroundPass ++ " show " ++ escapedPassLabel passLabel ++ " | " ++ extractUsername ++ " | " ++ stdinToClip

clipOTP :: String -> X ()
clipOTP passLabel = spawn $ workaroundPass ++ " otp " ++ escapedPassLabel passLabel ++ " | " ++ stdinToClip

typeOTP :: String -> X ()
typeOTP passLabel = spawn $ workaroundPass ++ " otp " ++ escapedPassLabel passLabel ++ " | " ++ typeWhatsInStdin

passGenerate :: String -> String -> X ()
passGenerate passOpts passLabel = runInFishTermWithGPG_TTY $ "pass generate -c " ++ passOpts ++ " " ++ escapedPassLabel passLabel ++ " 30 ; sleep infinity"

removePassword :: String -> X ()
removePassword passLabel = runInFishTermWithGPG_TTY $ "pass rm " ++ escapedPassLabel passLabel

edit :: String -> X ()
edit passLabel = runInFishTermWithGPG_TTY $ "pass edit " ++ escapedPassLabel passLabel

typePassword :: String -> X ()
typePassword passLabel = spawn $ workaroundPass ++ " show " ++ escapedPassLabel passLabel ++ " | " ++ extractPassword ++ " | " ++ typeWhatsInStdin

typeUsername passLabel = spawn $ workaroundPass ++ " show " ++ escapedPassLabel passLabel ++ " | " ++ extractUsername ++ " | " ++ typeWhatsInStdin

autofill :: String -> X ()
autofill passLabel = spawn $ "IFS= txt=$("++workaroundPass++" show " ++ escapedPassLabel passLabel ++ ") && echo $txt |"++extractUsername++"|"++ typeWhatsInStdin ++" && xdotool key Tab && echo $txt |"++extractPassword++"|" ++ typeWhatsInStdin


-- | UTILS below

runInFishTermWithGPG_TTY toRun = runInTerm alacrittyFloatingOpt $ "/usr/bin/env fish -c 'set -x GPG_TTY (tty);" ++ toRun ++ "'"

showInVimScratchPad = "vim --cmd \"set nowrap|setlocal bt=nofile\" -"
escapedPassLabel passLabel = "\""++ escapeQuote passLabel ++ "\""
typeWhatsInStdin = "tr -d '\n'|xdotool type --clearmodifiers --file -"
extractUsername = "grep -oP 'username: \\K.*'"
extractPassword = "head -n1"
workaroundPass = "export GPG_TTY='workaround'; pass"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = ['\\', '\"']
        escape x = return x

-- | Retrieve the list of passwords from the password store 'passwordStoreDir
getPasswords :: FilePath -> IO [String]
getPasswords passwordStoreDir = do
  files <- runProcessWithInput "find" [
    "-L", -- Traverse symlinks
    passwordStoreDir,
    "-type", "f",
    "-name", "*.gpg",
    "-printf", "%P\n"] []
  return . map removeGpgExtension $ lines files

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
