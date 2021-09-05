module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Socket

{- "HLint\: ignore Redundant bracket" -}

newtype CreateContainerOptions
  = CreateContainerOptions
    { image :: Image
    }

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"

  let body = Aeson.Null -- TODO: figure out body
  let request = HTTP.defaultRequest
              & HTTP.setRequestManager manager
              & HTTP.setRequestPath "/v1.40/containers/create"
              & HTTP.setRequestMethod "POST"
              & HTTP.setRequestBodyJSON body

  response <- HTTP.httpBS request

  traceShowIO response

newtype Image = Image Text
  deriving (Eq, Show)

imageToText :: Docker.Image -> Text
imageToText (Docker.Image image) = image

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code