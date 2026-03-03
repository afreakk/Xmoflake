module SpawnWorkspaceHook (shiftToSpawnerWorkspace) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as S
import Text.Read (readMaybe)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties (getProp32)

shiftToSpawnerWorkspace :: ManageHook
shiftToSpawnerWorkspace = do
  w <- ask
  mWs <- liftX $ do
    workspaceByPid <- buildWorkspaceByPidExcluding w
    mPid <- getWindowPid w
    case mPid of
      Nothing -> return Nothing
      Just pid -> do
        byParent <- io $ do
          mParentPid <- readParentPid pid
          case mParentPid of
            Nothing -> return Nothing
            Just parentPid -> findWorkspaceInAncestry workspaceByPid parentPid
        case byParent of
          Just ws -> return (Just ws)
          Nothing -> do
            mSpawnerWindow <- io $ findWindowIdInAncestry pid
            case mSpawnerWindow of
              Nothing -> return Nothing
              Just spawnerWindow -> withWindowSet $ \ws -> return (W.findTag spawnerWindow ws)
  maybe idHook doShift mWs

getWindowPid :: Window -> X (Maybe Int)
getWindowPid w = do
  pidAtom <- getAtom "_NET_WM_PID"
  prop <- getProp32 pidAtom w
  return $ fromIntegral <$> (prop >>= listToMaybe)

buildWorkspaceByPidExcluding :: Window -> X (M.Map Int WorkspaceId)
buildWorkspaceByPidExcluding excludedWindow = withWindowSet $ \ws -> do
  let workspaceWindows =
        [ (win, W.tag workspace)
          | workspace <- W.workspaces ws,
            W.tag workspace /= "NSP",
            win <- W.integrate' (W.stack workspace),
            win /= excludedWindow
        ]
  pidPairs <- mapM windowPidPair workspaceWindows
  return . M.fromList $ mapMaybe id pidPairs
  where
    windowPidPair :: (Window, WorkspaceId) -> X (Maybe (Int, WorkspaceId))
    windowPidPair (win, workspaceTag) = do
      mPid <- getWindowPid win
      return $ fmap (\pid -> (pid, workspaceTag)) mPid

findWorkspaceInAncestry :: M.Map Int WorkspaceId -> Int -> IO (Maybe WorkspaceId)
findWorkspaceInAncestry workspaceByPid = go S.empty
  where
    go :: S.Set Int -> Int -> IO (Maybe WorkspaceId)
    go seen pid
      | pid <= 1 = return Nothing
      | S.member pid seen = return Nothing
      | otherwise =
          case M.lookup pid workspaceByPid of
            Just ws -> return (Just ws)
            Nothing -> do
              mParentPid <- readParentPid pid
              case mParentPid of
                Nothing -> return Nothing
                Just parentPid -> go (S.insert pid seen) parentPid

readParentPid :: Int -> IO (Maybe Int)
readParentPid pid = do
  let statusPath = "/proc/" ++ show pid ++ "/status"
  statusResult <- (try (readFile statusPath) :: IO (Either IOException String))
  return $ either (const Nothing) extractPPid statusResult
  where
    extractPPid :: String -> Maybe Int
    extractPPid statusContent = do
      ppidLine <- find (("PPid:" `elem`) . words . take 5) (lines statusContent)
      readMaybe . dropWhile isSpace . drop (length ("PPid:" :: String)) $ ppidLine

findWindowIdInAncestry :: Int -> IO (Maybe Window)
findWindowIdInAncestry = go S.empty
  where
    go :: S.Set Int -> Int -> IO (Maybe Window)
    go seen pid
      | pid <= 1 = return Nothing
      | S.member pid seen = return Nothing
      | otherwise = do
          mWindow <- readWindowIdEnv pid
          case mWindow of
            Just windowId -> return (Just windowId)
            Nothing -> do
              mParentPid <- readParentPid pid
              case mParentPid of
                Nothing -> return Nothing
                Just parentPid -> go (S.insert pid seen) parentPid

readWindowIdEnv :: Int -> IO (Maybe Window)
readWindowIdEnv pid = do
  let environPath = "/proc/" ++ show pid ++ "/environ"
  environResult <- (try (readFile environPath) :: IO (Either IOException String))
  return $ either (const Nothing) extractWindowId environResult
  where
    extractWindowId :: String -> Maybe Window
    extractWindowId environContent = do
      envEntry <- find ("WINDOWID=" `isPrefixOf`) (splitByNull environContent)
      value <- stripPrefix "WINDOWID=" envEntry
      fromIntegral <$> (readMaybe value :: Maybe Integer)

splitByNull :: String -> [String]
splitByNull [] = []
splitByNull content =
  let (entry, rest) = break (== '\0') content
   in case rest of
        [] -> [entry]
        (_ : remainder) -> entry : splitByNull remainder
