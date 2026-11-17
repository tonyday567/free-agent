{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Process ports: stdin / stdout / stderr as free dual poles.
--
-- A 'StdPorts' handle is a persistent child process viewed through three
-- independent 'Circuit.Machine' seats plus a close action.  Pipes all the way
-- down: no FIFO, no log files, no byte-offset polling.
--
-- Internal I/O is 'ByteString'.  Constructors take encode/decode adapters:
--
-- @
-- openStdPorts encodeUtf8 decodeUtf8 cfg  -- 'Text'
-- openStdPorts id id cfg                  -- 'ByteString'
-- @
--
-- = Stream marks
--
-- The boundary grammar of a process stream is a type, 'ProcMarks' — the
-- level-0 grammar of 'Free.Agent.Mark' surfaced at the process boundary.
-- A mark announces the end of a frame; everything before it is the payload.
-- 'splitFrame' is the stateless parse of the mark machine: the byte buffer
-- is the whole state.
--
-- A pump thread per output handle frames the raw stream and closes each
-- payload into an internal FIFO.  The emit ends are the queue read legs: an
-- emit /blocks/ until a complete frame arrives — the queue's
-- @readTQueue@ retry IS the blocking boundary.  Arrival is decided by
-- content, never inferred from quiet; there is no timeout and no empty
-- poll result on the emit side.
--
-- = Resource lifecycle
--
-- 'openStdPorts' returns a @'Loop' 'Either'@ where the feedback state is
-- the process.  'Left' = process alive, 'Right' = ports delivered.  The
-- 'StdPorts' ends are self-contained — they capture the pipe handles, the
-- queues, and the pump threads.  'stdClose' terminates the process, reaps
-- the exit (no zombies), kills the pumps, and closes the handles.
--
-- = Death is content
--
-- A dead child goes quiet today-or-never: the pumper turns EOF or
-- 'IOException' into exactly one terminal frame ('Exited' with the exit
-- code, after 'waitForProcess'), sunk into the same queue as the
-- dialogue.  A read blocked while the child lives therefore ends by
-- content when the child dies — the terminal frame is the last token.
module Free.Agent.StdPorts
  ( ProcConfig (..),
    ProcMarks (..),
    splitFrame,
    lineMarks,
    hermesMarks,
    sseMarks,
    Frame (..),
    StdPorts (..),
    openStdPorts,
    frameAgent,
    echo,
    DoubleStep (..),
    scriptedDouble,
  )
where

import Circuit.Category (K (..))
import Circuit.Machine (Body (Body), Moore, Poles (..))
import Circuit.Syntax (Syntax (Lift))
import Circuit.Tensor (Const (..), unConst)
import Circuit.Trace (FreeTrace)
import Circuit.Traced (trace)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, newTBQueueIO, newTQueueIO, readTBQueue, readTQueue, tryReadTBQueue, writeTBQueue, writeTQueue)
import Control.Exception (IOException, onException, try)
import Control.Monad (forever, unless, void)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.Exit (ExitCode (..))
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    hClose,
    hFlush,
    hSetBuffering,
  )
import System.Process
import Prelude hiding (read)

-- $setup
--
-- >>> import Circuit.Machine (MooreT (..), scan)
-- >>> import Circuit.Machine (Poles (..))
-- >>> import Circuit.Category (K (..))
-- >>> import Data.Text.Encoding (decodeUtf8)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data ProcConfig = ProcConfig
  { procCommand :: String,
    procArgs :: [String],
    procWorkingDir :: FilePath,
    -- | Full child environment; 'Nothing' inherits the launcher's environment.
    procEnv :: Maybe [(String, String)],
    -- | Boundary grammar of the stdout stream.  Stderr is line-framed
    -- ('lineMarks'); stderr is diagnostics, not dialogue.
    procMarks :: ProcMarks,
    -- | Merge stderr into the stdout pipe: one pipe for both streams, so
    -- diagnostics are ordered against the dialogue by the child itself.
    -- The stderr pole has nothing of its own in this mode.
    procMergeStderr :: Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Stream marks
-- ---------------------------------------------------------------------------

-- | The boundary grammar of a process stream: a finite set of marks, each
-- a glyph sequence announcing the end of a frame.  This is the level-0
-- grammar of the process boundary — the free boundary @K + payload@ with
-- @K@ finite, the stateless 'splitFrame' of the mark machine.  Anything
-- stateful (turn counters, roles) lives above this layer.
newtype ProcMarks = ProcMarks [Text]
  deriving (Show, Eq)

-- | Line framing: the newline is the mark, one frame per line.  For line
-- protocols (ACP's JSON-RPC) and for stderr diagnostics.
lineMarks :: ProcMarks
lineMarks = ProcMarks ["\n"]

-- | The hermes CLI prompt grammar: @❯@ between separator lines when ready.
-- ANSI decorations ride inside the payload; the mark itself is a bare glyph.
-- (Probe card: @coffee\/loom\/hermes-boundary-probe.md@.)
hermesMarks :: ProcMarks
hermesMarks = ProcMarks ["❯"]

-- | The SSE (Server-Sent Events) level-0 grammar: the blank line ends an
-- event frame.  Payload is the @event:\/data:@ block, mark stripped.
sseMarks :: ProcMarks
sseMarks = ProcMarks ["\n\n"]

-- | The stateless mark parse: the earliest mark occurrence in the buffer,
-- giving @(payload, rest)@ — payload before the mark (mark stripped), rest
-- after it.  'Nothing' when no mark has arrived yet.
--
-- Marks are matched on bytes ('encodeUtf8').  UTF-8 is self-synchronising:
-- a valid multi-byte encoding never occurs inside another character, so a
-- byte-level mark cannot false-match, and a mark split across read chunks
-- is found once the buffer accumulates its final byte.
--
-- Chunking invariance: the frames of a chunked stream equal the frames of
-- its concatenation, for every cut, iff no mark is a proper prefix of
-- another mark.  (Counterexample: marks @[\"\\n\\n\", \"\\n\"]@, input
-- @"a\\n\\nb"@ cut after @"a\\n"@ yields @[\"a\", \"\", \"b\"]@ instead of
-- @[\"a\", \"b\"]@ — the short mark fires before the long one can span the
-- chunk boundary.)  The shipped grammars satisfy the condition — pinned
-- by the S1 oracles — so no longest-match hold-back is implemented.
splitFrame :: ProcMarks -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitFrame (ProcMarks marks) buf =
  case hits of
    [] -> Nothing
    _ ->
      let (_, markBytes, before, rest) = minimumBy (comparing (\(n, _, _, _) -> n)) hits
       in Just (before, BS.drop (BS.length markBytes) rest)
  where
    hits =
      [ (BS.length before, markBytes, before, rest)
      | mark <- marks,
        let markBytes = encodeUtf8 mark,
        not (BS.null markBytes),
        let (before, rest) = BS.breakSubstring markBytes buf,
        markBytes `BS.isPrefixOf` rest
      ]

-- ---------------------------------------------------------------------------
-- Process ports
-- ---------------------------------------------------------------------------

-- | A frame on a process stream: payload content, or the terminal
-- exit-as-frame.  The child's death rides the same pole as the dialogue:
-- exactly one 'Exited' frame, after every flushed payload, so a read
-- blocked on a dying child ends by content instead of hanging.
data Frame a = Payload a | Exited ExitCode
  deriving (Eq, Show, Functor)

-- | A process token with three matched poles: stdin write, stdout read,
-- and stderr read.
--
-- The read poles block: a read returns the next complete frame, waiting on
-- the queue when the stream has not produced one yet.  There is no
-- empty-read result — quiet is not an opinion, and death is a frame.
data StdPorts a b c = StdPorts
  { stdIn :: Poles () (K IO) a (),
    stdOut :: Poles () (K IO) () (Frame b),
    stdErr :: Poles () (K IO) () (Frame c),
    stdClose :: IO (),
    -- | The child's handle, where there is one — in-memory doubles have
    -- no process.  After 'stdClose' the exit is reaped, so
    -- 'System.Process.getProcessExitCode' returns 'Just'.
    stdProcess :: Maybe ProcessHandle
  }

-- | Spawn a process and open its ports as a 'FreeTrace' 'Either'.
--
-- @encode@ converts a token to bytes written to stdin (+ newline).
-- @decode@ converts a framed payload's bytes back to a token.
--
-- The feedback state is the process.  'Left' = process alive, 'Right' =
-- ports delivered, resources captured by 'stdClose'.  If setup fails
-- after the spawn, the child is terminated and reaped — no orphans.
openStdPorts ::
  (a -> BS.ByteString) ->
  (BS.ByteString -> a) ->
  ProcConfig ->
  FreeTrace Either (K IO) () (StdPorts a a a)
openStdPorts encode decode cfg = trace (Lift (K step))
  where
    step (Right ()) = do
      let procSpec =
            (proc (procCommand cfg) (procArgs cfg))
              { cwd = Just (procWorkingDir cfg),
                env = procEnv cfg,
                std_in = CreatePipe
              }
      if procMergeStderr cfg
        then do
          -- One pipe for both streams: the kernel preserves the child's
          -- write order, so diagnostics are ordered against the dialogue.
          (readEnd, writeEnd) <- createPipe
          (Just stdinH, _, _, ph) <-
            createProcess
              procSpec
                { std_out = UseHandle writeEnd,
                  std_err = UseHandle writeEnd
                }
          flip onException (terminateProcess ph >> void (waitForProcess ph)) $ do
            hClose writeEnd
            hSetBuffering stdinH NoBuffering
            hSetBuffering readEnd NoBuffering
            qOut <- openFIFO :: IO (Poles () (K IO) (Frame a) (Frame a))
            outTid <- forkIO (pumpFrames (procMarks cfg) decode readEnd ph (sink qOut))
            let writeIn =
                  K $ \a -> do
                    BS.hPutStr stdinH (encode a <> "\n")
                    hFlush stdinH
                noWrite = K $ \_ -> pure ()
                blockForever = K $ \_ -> forever (threadDelay maxBound)
                ports =
                  StdPorts
                    { stdIn = Poles writeIn writeIn noWrite,
                      stdOut = Poles noWrite noWrite (emit qOut),
                      stdErr = Poles noWrite noWrite blockForever,
                      stdClose = do
                        void $ try @IOException (hClose stdinH)
                        terminateProcess ph
                        _ <- waitForProcess ph
                        killThread outTid,
                      stdProcess = Just ph
                    }
            pure (Left ports)
        else do
          (Just stdinH, Just stdoutH, Just stderrH, ph) <-
            createProcess
              procSpec
                { std_out = CreatePipe,
                  std_err = CreatePipe
                }
          flip onException (terminateProcess ph >> void (waitForProcess ph)) $ do
            hSetBuffering stdinH NoBuffering
            hSetBuffering stdoutH NoBuffering
            hSetBuffering stderrH NoBuffering

            -- Mark-carrying stdout uses the linear (empty-residual) mediator so
            -- halt marks are preserved in order — now over 'Frame', so the
            -- terminal frame cannot be dropped either.  Diagnostic stderr uses a
            -- bounded weakening mediator: old diagnostics may be dropped when
            -- the reader falls behind, but the process dialogue must never lose
            -- a halt.
            qOut <- openFIFO :: IO (Poles () (K IO) (Frame a) (Frame a))
            qErr <- openNewest 100 :: IO (Poles () (K IO) (Frame a) (Frame a))
            outTid <- forkIO (pumpFrames (procMarks cfg) decode stdoutH ph (sink qOut))
            errTid <- forkIO (pumpFrames lineMarks decode stderrH ph (sink qErr))

            let writeIn =
                  K $ \a -> do
                    BS.hPutStr stdinH (encode a <> "\n")
                    hFlush stdinH
                noWrite = K $ \_ -> pure ()
                ports =
                  StdPorts
                    { stdIn = Poles writeIn writeIn noWrite,
                      stdOut = Poles noWrite noWrite (emit qOut),
                      stdErr = Poles noWrite noWrite (emit qErr),
                      stdClose = do
                        void $ try @IOException (hClose stdinH)
                        terminateProcess ph
                        _ <- waitForProcess ph
                        killThread outTid
                        killThread errTid,
                      stdProcess = Just ph
                    }
            pure (Left ports)
    step (Left ports) =
      pure (Right ports)

-- | An unbounded FIFO, used for dialogue and terminal frames.
openFIFO :: IO (Poles () (K IO) a a)
openFIFO = do
  q <- newTQueueIO
  let write = K (atomically . writeTQueue q)
      read' = K (const (atomically (readTQueue q)))
  pure (Poles write write read')

-- | A bounded diagnostic queue which drops the oldest value when full.
openNewest :: Int -> IO (Poles () (K IO) a a)
openNewest n = do
  q <- newTBQueueIO (fromIntegral n)
  let writeOne x = writeTBQueue q x <|> (tryReadTBQueue q *> writeTBQueue q x)
      write = K (atomically . writeOne)
      read' = K (const (atomically (readTBQueue q)))
  pure (Poles write write read')

-- | Commit one token to a queue end.
sink :: Poles () (K IO) a a -> a -> IO ()
sink q = runK (commit q)

-- | The pumper as an agent: a Moore machine from maybe-chunks to frame
-- lists.  The carrier is @(buffer, pending)@ — the unexplained suffix and
-- the frames awaiting observation.  @Just chunk@ is the percept,
-- 'Nothing' the end-of-stream mark: the EOF flush is content, decided by
-- the same stateless grammar.  This is the level-0 mark machine made
-- literal; 'pumpFrames' is merely its IO interpretation at a 'Handle'.
--
-- >>> scan (MooreT (frameAgent lineMarks decodeUtf8)) [Just "a\n", Just "b", Nothing]
-- [["a"],[],["b"]]
frameAgent :: ProcMarks -> (BS.ByteString -> a) -> Moore (,) (BS.ByteString, [a]) (->) (Maybe BS.ByteString) [a]
frameAgent marks decode = Body (\i -> step ((BS.empty, []), i)) step (Const . snd)
  where
    step ((buf, _), i) = case i of
      Just chunk ->
        let (fs, buf') = peel (buf <> chunk)
         in (buf', map decode fs)
      Nothing ->
        (BS.empty, [decode buf | not (BS.null buf)])
    peel buf = case splitFrame marks buf of
      Nothing -> ([], buf)
      Just (p, rest) ->
        let (ps, rest') = peel rest
         in (p : ps, rest')

-- | One body step: the observation of the post-step state, with the state.
runCell :: Moore (,) s (->) i o -> s -> i -> (o, s)
runCell (Body _ k o) s i = let s' = k (s, i) in (unConst (o s'), s')

-- | The pumper: the IO interpretation of 'frameAgent' at a 'Handle'.
-- Blocking reads deliver percepts; payloads are sunk into the queue as
-- 'Payload' frames.  A mark split across reads completes in the buffer;
-- end-of-stream is the 'Nothing' percept, flushing a partial frame as
-- the final payload, after which the exit code is collected and sunk as
-- exactly one terminal 'Exited' frame.
pumpFrames :: ProcMarks -> (BS.ByteString -> a) -> Handle -> ProcessHandle -> (Frame a -> IO ()) -> IO ()
pumpFrames marks decode h ph snk = go (BS.empty, [])
  where
    sys = frameAgent marks decode
    go st = do
      r <- try @IOException (BS.hGetSome h 4096)
      let input = case r of
            Left _ -> Nothing
            Right bs
              | BS.null bs -> Nothing
              | otherwise -> Just bs
          (outs, st') = runCell sys st input
      traverse_ (snk . Payload) outs
      case input of
        Just _ -> go st'
        Nothing -> do
          code <- waitForProcess ph
          snk (Exited code)

-- ---------------------------------------------------------------------------
-- In-memory test harness
-- ---------------------------------------------------------------------------

-- | An in-memory echo repl: write a token, read the transformed result.
-- No process, no files.  For fast bus-connector testing.
--
-- The double models reality: a read before any write /blocks/ (quiet is
-- not an error), and 'stdClose' is a no-op — an in-memory double has no
-- process to reap.  See 'scriptedDouble' for death, silence, and bursts.
--
-- >>> pp <- echo pure :: IO (StdPorts String String ())
-- >>> runK (commit (stdIn pp)) "hi"
-- >>> runK (emit (stdOut pp)) ()
-- Payload "hi"
echo :: (a -> IO a) -> IO (StdPorts a a ())
echo f = do
  q <- newTQueueIO
  let writeIn =
        K $ \a -> do
          result <- f a
          atomically (writeTQueue q (Payload result))
      noWrite = K $ \_ -> pure ()
      readOut = K $ \_ -> atomically (readTQueue q)
      readErr = K $ \_ -> forever (threadDelay maxBound)
      ports =
        StdPorts
          { stdIn = Poles writeIn writeIn noWrite,
            stdOut = Poles noWrite noWrite readOut,
            stdErr = Poles noWrite noWrite readErr,
            stdClose = pure (),
            stdProcess = Nothing
          }
  pure ports

-- | One scripted step of an in-memory double.
data DoubleStep a
  = -- | Answer the written token with the transformed token.
    Reply (a -> a)
  | -- | Answer nothing: the read blocks, like a quiet child.
    Quiet
  | -- | Die: the read gets the terminal 'Exited' frame, once.
    Die ExitCode
  | -- | Answer one token with two frames, like a chatty child.
    Reply2 (a -> a) (a -> a)

-- | A scripted in-memory double on the same pole shapes as a real port:
-- each written token advances the script by one step.  After 'Die' the
-- terminal frame is delivered once and further reads block, matching the
-- real pump.  The script repeats from the start when exhausted.
--
-- This is the honest double the harness was missing: block, die, emit
-- zero frames, emit two frames — everything oracled against the same
-- 'Frame' vocabulary as a live process.
scriptedDouble :: [DoubleStep a] -> IO (StdPorts a a c)
scriptedDouble script = do
  q <- newTQueueIO
  cursor <- newIORef 0
  dead <- newIORef False
  let step a = do
        d <- readIORef dead
        unless d $
          unless (null script) $ do
            i <- readIORef cursor
            writeIORef cursor ((i + 1) `mod` length script)
            case drop i script of
              (Reply g : _) -> atomically (writeTQueue q (Payload (g a)))
              (Quiet : _) -> pure ()
              (Die code : _) -> do
                writeIORef dead True
                atomically (writeTQueue q (Exited code))
              (Reply2 g1 g2 : _) ->
                atomically $ do
                  writeTQueue q (Payload (g1 a))
                  writeTQueue q (Payload (g2 a))
              [] -> pure ()
      writeIn = K step
      noWrite = K $ \_ -> pure ()
      readOut = K $ \_ -> atomically (readTQueue q)
      readErr = K $ \_ -> forever (threadDelay maxBound)
      ports =
        StdPorts
          { stdIn = Poles writeIn writeIn noWrite,
            stdOut = Poles noWrite noWrite readOut,
            stdErr = Poles noWrite noWrite readErr,
            stdClose = pure (),
            stdProcess = Nothing
          }
  pure ports
