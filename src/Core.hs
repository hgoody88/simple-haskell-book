module Core where

import RIO
    ( ($),
      Eq((==)),
      Ord((>)),
      Show,
      Applicative(pure),
      Int,
      IO,
      Either(..),
      NonEmpty,
      Text,
      Map,
      undefined,
      Maybe(..) )
import RIO.Map
  ( empty
  , insert
  , notMember
  )
import RIO.List
  ( all
  , find
  )
import qualified Data.List.NonEmpty as NE

import qualified Docker

{- "HLint\: ignore Redundant bracket" -}

newtype Pipeline
  = Pipeline
      { steps :: NonEmpty Step
      }
  deriving (Eq, Show)

data Step
  = Step
      { name :: StepName
      , commands :: NonEmpty Text
      , image :: Docker.Image
      }
  deriving (Eq, Show)

data Build
  = Build
      { pipeline :: Pipeline
      , state :: BuildState
      , completedSteps :: Map StepName StepResult
      }
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype BuildRunningState
  = BuildRunningState
   { step :: StepName
   }
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build = do
  if allSucceeded
    then case nextStep of
      Nothing -> Left BuildSucceeded
      Just step -> Right step
    else Left BuildFailed
  where
    allSucceeded = all (StepSucceeded == ) build.completedSteps
    nextStep = find f (build.pipeline.steps) -- returns Maybe Step
    f step = notMember step.name build.completedSteps

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit = 
  if Docker.exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result -> pure $
          build { state = BuildFinished result }
        Right step -> do
          let brs = BuildRunningState step.name
          pure $ build { state = BuildRunning brs }
    BuildRunning brs -> do
      -- TODO: assuming step completes
      let exit = Docker.ContainerExitCode 0
          result = exitCodeToStepResult exit

      let name = brs.step

      pure build
        { state = BuildReady
        , completedSteps
          = insert (step brs) result build.completedSteps
        }

    BuildFinished _ ->
      pure build