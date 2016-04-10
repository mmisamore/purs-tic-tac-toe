module Main where

import Prelude (class Show, Unit, void, (>>>), bind, show, (++),
                return, (<$>), map, unit, (+), (*))
import Data.Array        (updateAt, (!!), replicate)
import Data.Maybe        (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable     (toMaybe)
import Control.Monad.Eff

import DOM               (DOM)
import DOM.Node.Types    (Element)
import DOM.HTML          (window)
import DOM.HTML.Window   (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types    (htmlElementToElement)

import React           (ReactElement,spec,createClass,createFactory)
import ReactDOM        (render)
import React.DOM       (text,div,div',button,td,tr',tbody',table')
import React.DOM.Props (className,onClick)

import Signal as S
import Signal.Channel (CHANNEL, Channel, subscribe, channel, send)

data Token = X | O | E

instance showToken :: Show Token where
  show X = "X"
  show O = "O"
  show E = "E"

classForToken :: Token -> String
classForToken X = "cell x"
classForToken O = "cell o"
classForToken E = "cell e"

-- Game actions
data Action = NewGame | Click Int Int

-- Game board
type Board = Array Token
newBoard :: Board
newBoard = replicate 9 E 

-- Game state consists of board and current player
type State a = { board :: Board, currentPlayer :: Token | a }

-- New game is just a new board with current player X
type GameState = State ()
newGameState :: GameState
newGameState = { board: newBoard, currentPlayer: X }

-- Game environment is a state together with an action channel
type Environment = State (channel :: Channel Action)

mkEnv :: Channel Action -> GameState -> Environment
mkEnv channel gameState = {
  board: gameState.board,
  currentPlayer: gameState.currentPlayer,
  channel: channel
}

-- Try to get token from board given board position
get :: Int -> Int -> Board -> Maybe Token
get x y board = board !! (3 * x + y)

-- Set token on board
set :: Int -> Int -> Token -> Board -> Board
set x y token board = fromJust (updateAt (3*x + y) token board)

-- Determine the next player from a Token
nextPlayer :: Token -> Token
nextPlayer X = O
nextPlayer O = X
nextPlayer _ = X

-- Basic game step
gameStep :: Action -> GameState -> GameState
gameStep NewGame _ = newGameState
gameStep (Click x y) gameState =
  case get x y gameState.board of
    Just E -> gameState {
      board = set x y gameState.currentPlayer gameState.board,
      currentPlayer = nextPlayer gameState.currentPlayer
    }
    _ -> gameState

-- Render the board 
grid :: Environment -> ReactElement
grid env = table' [tbody' (map (row env) [0,1,2])]

-- Render a row
row :: Environment -> Int -> ReactElement
row env x = tr' (map (cell env x) [0,1,2])

-- Render a cell, assuming correct coordinates
cell :: Environment -> Int -> Int -> ReactElement
cell env x y = td
  [className (classForToken token),
   onClick (\_ -> send env.channel (Click x y))]
  [text (show token)]
  where
    token = fromJust (get x y env.board)

-- Button to start a new game
newGameButton :: Channel Action -> ReactElement
newGameButton c = button 
  [onClick (\_ -> send c NewGame)]
  [text "New Game"] 

-- Extract the body element from the ambient DOM
getBody :: forall r. Eff (dom :: DOM | r) Element
getBody = do
  win <- window
  doc <- document win
  bodyElt <- fromJust <$> toMaybe <$> body doc
  return (htmlElementToElement bodyElt) 

-- The game layout itself
game :: Environment -> ReactElement
game env = div
  [className "gameClass"]
  [newGameButton env.channel,
   grid env,
   text (show (env.currentPlayer) ++ "'s turn")]

-- Generate the React class for the game 
app :: Environment -> ReactElement 
app env = div' [ createFactory reactClass unit ] 
  where reactClass = createClass (spec unit \_ -> return (game env))

-- Start it up
main :: forall r. Eff (dom :: DOM, channel :: CHANNEL | r) Unit
main = do
   -- Get DOM body so we can mount to it
   body <- getBody

   -- Setup communication and Game state
   channel <- channel NewGame
   let actions = subscribe channel
   let gameState = S.foldp gameStep newGameState actions

   -- The game Signal
   let gameS = 
     -- Over the State signal
     gameState S.~> 
     -- Add the channel so that we can generate new events
     mkEnv channel >>> 
     -- Use the current state to render to the body
       (\env -> render (app env) body) >>> void

   -- Run the Game
   S.runSignal gameS
   

