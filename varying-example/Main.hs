{-# LANGUAGE TupleSections #-}
module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Buttons
import Control.Varying
import Control.Applicative
import Control.Monad.IO.Class
import Data.Function (fix)
import Data.Maybe (isJust)

-- | Just for fun we can run two dynamic streams both at the same time, adding
-- them and then dividing by two, this way we can check that both signals are
-- working and equal.
combineDyns :: (Applicative m, MonadIO m)
            => VarT m G.Event Int -> VarT m G.Event Int
            -> VarT m G.Event Int
combineDyns a b = floor <$> (fromIntegral <$> (a + b)) / 2

-- | Simply produces "-1" if the second signal produces False, otherwise it
-- produces the value of the first signal.
negWhenUntoggled :: (Applicative m, Monad m)
               => VarT m G.Event Int -> VarT m G.Event Bool -> VarT m G.Event Int
negWhenUntoggled count mode = (\n on -> if on then n else -1) <$> count <*> mode
--------------------------------------------------------------------------------
-- Streams/Signals that everything needs
--------------------------------------------------------------------------------
-- | An event stream that procs every time the input contains a button click.
clicked :: (Applicative m, Monad m)
        => (G.Event -> Maybe ButtonClick) -> VarT m G.Event (Event ())
clicked f = var ((== Just Click)  . f) >>> onTrue

-- | An event stream that procs every time the input contains a button toggle.
toggle :: (Applicative m, Monad m)
       => (G.Event -> Maybe ButtonClick) -> VarT m G.Event (Event ())
toggle f = var ((== Just Toggle) . f)  >>> onTrue

-- | A stream that produces whether or not the input has toggled on or off.
-- Produces 'True' when counters should increment and 'False' when the should
-- display (-1).
toggled :: (Applicative m, Monad m)
        => (G.Event -> Maybe ButtonClick) -> VarT m G.Event Bool
toggled f = toggle f >>> accumulate g True
  where g on e = if isJust e then not on else on

-- | Simply count any events that cross the input.
countEvents :: (Applicative m, Monad m) => Int -> VarT m (Event a) Int
countEvents = foldStream (\n _ -> n + 1)
--------------------------------------------------------------------------------
-- Static
--------------------------------------------------------------------------------
static0 :: (Applicative m, Monad m) => VarT m G.Event Int
static0 = negWhenUntoggled (events >>> accumulate acc 0) $ toggled filter0
  where events = (,,) <$> toggled filter0 <*> toggle filter0 <*> clicked filter0
        acc n (False,       _,       _) = -1
        acc n ( True, Just (),       _) = 0
        acc n ( True, Nothing, Just ()) = n + 1
        acc n _ = n

static5 :: (Applicative m, Monad m) => VarT m G.Event Int
static5 = negWhenUntoggled signal $ toggled filter5
  where events = toggled filter5 >>> pure (Just ())
        signal = events >>> countEvents 0

static10 :: (Applicative m, Monad m) => VarT m G.Event Int
static10 = negWhenUntoggled signal $ toggled filter10
  where signal = clicked filter10 >>> countEvents 0
--------------------------------------------------------------------------------
-- Dynamic 0 in continuous and spline fashion
--------------------------------------------------------------------------------
-- | Using splines (a monadic layer on top of event streams) we can piece
-- together the steps in a programmer friendly way and then use "outputStream"
-- to connect it all into a continuous stream. This makes the signal flow easy
-- to understand. Here we're running with IO in the stack so we can print
-- some trace statements.
dyn0Spline :: (Applicative m, MonadIO m) => VarT m G.Event Int
dyn0Spline = flip outputStream 0 $ fix $ \loop -> do
  liftIO $ putStrLn "counting clicks..."
  let countClicks = clicked filter0 >>> countEvents 0
  -- Count clicks until the user hits the toggle button
  clicks <- countClicks `untilEvent_` toggle filter0
  liftIO $ putStrLn $ "got " ++ show clicks ++ " clicks before toggle"
  -- Produce (-1) exactly once when the toggle button was hit.
  step (-1)
  liftIO $ putStrLn "stepped -1 and now waiting for another toggle..."
  -- Produce (-1) until the user hits the toggle button again.
  (-1) `_untilEvent_` toggle filter0
  liftIO $ putStrLn "got a toggle"
  -- Produce 0 exactly when the toggle button was hit.
  step 0
  liftIO $ putStrLn "stepped 0"
  -- Loop over again
  loop
--------------------------------------------------------------------------------
-- Dynamic 5 & 10
--------------------------------------------------------------------------------
dyn5Var :: (Applicative m, Monad m) => VarT m G.Event Int
dyn5Var = negWhenUntoggled switcher (toggled filter5)
  where switcher = (count `onlyWhenE` toggledOn) >>> startingWith 0
        count = clicked filter5 >>> countEvents 0
        toggledOn = toggled filter5 >>> onTrue

dyn5Spline :: (Applicative m, Monad m) => VarT m G.Event Int
dyn5Spline = outputStream (loop 0) 0
  where countClicksFrom = (clicked filter5 >>>) . countEvents
        loop n = do
          clicks <- countClicksFrom n `untilEvent_` toggle filter5
          step (-1)
          (-1) `untilEvent_` toggle filter5
          step clicks
          loop clicks

--------------------------------------------------------------------------------
-- Putting it all together
--------------------------------------------------------------------------------
main :: IO ()
main = G.playIO (InWindow "Varying Example" (320, 240) (800, 200)) white 30
                (renderButtons 0 (Just 0) 0 (Just 0) 0 (Just 0), network)
                (return . fst) (\e (_, network') -> runVarT network' e)
                (const return)

network :: (Applicative m, MonadIO m) => VarT m G.Event Picture
network =
  renderButtons <$> static0
                <*> (Just <$> combineDyns dyn0Var dyn0Spline)
                <*> negWhenUntoggled static5 (toggled filter5)
                <*> (Just <$> combineDyns dyn5Var dyn5Spline)
                <*> negWhenUntoggled static10(toggled filter10)
                <*> pure Nothing

