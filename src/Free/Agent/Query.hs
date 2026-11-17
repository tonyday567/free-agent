{-# LANGUAGE OverloadedStrings #-}

-- | Generic query-to-agent adapters.
--
-- Turns any @Text -> IO Text@ boundary into an input-seeded 'Agent' that
-- consumes addressed posts and emits reply posts. The functions here know
-- nothing about external CLI processes, session files, or concrete binaries.
module Free.Agent.Query
  ( sessionPrompt,
    replyPosts,
    synthesisPosts,
    queryBody,
    queryBodyWith,
    synthBody,
    echoBody,
  )
where

import Circuit.Post (Post (..), PostId, mkPost, replyTo, sortNub, synthesis)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Agent (Agent, ioAgent)

-- | Session assembly for the opaque seat: bodies, oldest-first, one per line.
sessionPrompt :: [Post Text Text] -> Text
sessionPrompt = T.intercalate "\n" . map body

-- | Build reply posts from a cleaned agent response.
--
-- Addresses the last input's sender, preserves any other names on the
-- original wire, and threads onto the last input's 'PostId' when supplied.
replyPosts :: Text -> [Post Text Text] -> [PostId] -> Text -> [Post Text Text]
replyPosts who ins ids reply =
  case (listToMaybe (reverse ins), T.strip reply) of
    (_, r) | T.null r -> []
    (Nothing, _) -> []
    (Just lastIn, r) ->
      let to' = from lastIn : filter (/= who) (to lastIn)
       in case listToMaybe (reverse ids) of
            Just parentId -> [replyTo who parentId lastIn r]
            Nothing -> [mkPost who to' r]

-- | Build one synthesis post from a cleaned agent response.
synthesisPosts :: Text -> [Post Text Text] -> [PostId] -> Text -> [Post Text Text]
synthesisPosts who ins ids reply =
  case (ins, T.strip reply) of
    (_, r) | T.null r -> []
    ([], _) -> []
    (_, r) ->
      let audience = filter (/= who) (sortNub (concatMap (\p -> from p : to p) ins))
          parentIds = take (length ins) ids
       in [if null parentIds then mkPost who audience r else synthesis who audience parentIds r]

-- | Opaque evaluate seat. Commits collect the independent input posts; the
-- first emit runs the query once and returns its reply beside 'Done'.
queryBody :: Text -> [PostId] -> (Text -> IO Text) -> IO (Agent [Post Text Text])
queryBody who = queryBodyWith replyPosts who

-- | Opaque synthesis seat: the reply cites every supplied input id as ancestry.
synthBody :: Text -> [PostId] -> (Text -> IO Text) -> IO (Agent [Post Text Text])
synthBody who = queryBodyWith synthesisPosts who

-- | 'queryBody' parameterised on the reply-to-posts builder. Bare posts do
-- not carry log stamps, so the delivery runner supplies their ancestry.
queryBodyWith ::
  (Text -> [Post Text Text] -> [PostId] -> Text -> [Post Text Text]) ->
  Text ->
  [PostId] ->
  (Text -> IO Text) ->
  IO (Agent [Post Text Text])
queryBodyWith posts who ids query =
  pure $ ioAgent $ \ins -> do
    if null ins
      then pure []
      else do
        reply <- query (sessionPrompt ins)
        pure (posts who ins ids reply)

-- | Mock seat: reply body is the session prompt.
echoBody :: Text -> [PostId] -> IO (Agent [Post Text Text])
echoBody who ids = queryBody who ids pure
