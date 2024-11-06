module Main exposing (main)

import Playground exposing (..)



-- MEMORY


type alias Memory =
  { level : Level 
  , dog : Dog
  , bugs : List Bug
  }

type Level 
  = StartScreen
  | Level1


type alias Dog =
  { x : Number
  , y : Number
  , direction : String
  }

type alias Bug =
  { x : Number
  , y : Number
  }

startDog =
  { x = 0
  , y = 0
  , direction = "right"
  }

startBug =
  { x = 0
  , y = 0
  }

startMemory =
  { level = StartScreen 
  , dog = startDog
  , bugs = List.repeat 20 startBug
  }



-- MAIN


main =
  game view update startMemory
    


-- VIEW


view : Computer -> Memory -> List Shape
view computer memory =
  case memory.level of
    StartScreen ->
      viewBackground blue computer.screen
        ++  [  words white "Press the 'Enter' key to Start"
                |> scale 4
            ]

    Level1 ->
      viewBackground green computer.screen
        ++ List.map viewBug memory.bugs
        ++ [ viewDog memory.dog ]


viewBackground : Color -> Screen -> List Shape
viewBackground color screen =
  [ rectangle color screen.width screen.height
  ]


viewBug : Bug -> Shape
viewBug bug =
  rectangle black 64 64
    |> move bug.x bug.y


viewDog : Dog -> Shape
viewDog dog =
  image 96 96 (toDogGif dog)
    |> moveDog dog


-- UPDATE


update : Computer -> Memory -> Memory
update computer memory = 
  case memory.level of
    StartScreen ->
      if computer.keyboard.enter then
        { memory | 
            level = Level1 
        ,   bugs = List.map (assignRandomPosition computer.time) memory.bugs
        }
      else
        memory
    
    Level1 ->
      { memory | 
          dog = (updateDog computer memory.dog)
      ,   bugs = memory.bugs
      }
  

updateDog computer dog =
  let
    velocity = 3
  in
  { x = dog.x + (velocity * toX computer.keyboard)
  , y = dog.y + (velocity * toY computer.keyboard)
  , direction = chooseDirection computer dog
  }


-- HELPERS

assignRandomPosition : Time -> Bug -> Bug
assignRandomPosition time bug =
  { bug | 
      x = cos (spin 0.1 time)
  ,   y = sin (spin 0.1 time)
  }


chooseDirection computer dog =
  let 
    horizontalDirection = toX computer.keyboard
  in
  if horizontalDirection == 0 then 
    dog.direction
  else if horizontalDirection < 0 then
    "left"
  else
    "right"


moveDog dog =
  move dog.x dog.y


toDogGif dog =
  "/img/dog/" ++ dog.direction ++ ".gif"

