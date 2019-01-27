module PlanningGame.Api.Authorization.Type where


data AuthorizationError
  = SessionNotFound
  | SessionIdMissing
  deriving (Show, Eq)
