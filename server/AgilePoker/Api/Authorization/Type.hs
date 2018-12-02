module AgilePoker.Api.Authorization.Type where


data AuthorizationError
  = SessionNotFound
  | SessionIdMissing