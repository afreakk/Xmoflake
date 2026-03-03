module SpawnWorkspaceHook (shiftToSpawnerWorkspace) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.List (find)
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
    mPid <- getWindowPid w
    case mPid of
      Nothing -> return Nothing
      Just pid -> do
        workspaceByPid <- buildWorkspaceByPid
        io $ findWorkspaceInAncestry workspaceByPid pid
  maybe idHook doShift mWs

getWindowPid :: Window -> X (Maybe Int)
getWindowPid w = do
  pidAtom <- getAtom "_NET_WM_PID"
  prop <- getProp32 pidAtom w
  return $ fromIntegral <$> (prop >>= listToMaybe)

buildWorkspaceByPid :: X (M.Map Int WorkspaceId)
buildWorkspaceByPid = withWindowSet $ \ws -> do
  let workspaceWindows =
        [ (win, W.tag workspace)
          | workspace <- W.workspaces ws,
            W.tag workspace /= "NSP",
            win <- W.integrate' (W.stack workspace)
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
