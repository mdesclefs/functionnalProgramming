module Grammar where 

import qualified Parser
import qualified Control.Monad

data Color = Yellow | Red | Blue | Green deriving (Eq, Enum, Read, Show)
color :: Parser.Parser Color
color = do { Parser.string "yellow";
             return (Yellow)
            }
        `Parser.orelse`
        do { Parser.string "red";
             return (Red)
            }
        `Parser.orelse`
        do { Parser.string "blue";
             return (Blue)
            }
        `Parser.orelse`
        do { Parser.string "green";
             return (Green)
            }

data Control = Human | AI deriving (Eq, Enum, Read, Show)
control :: Parser.Parser Control
control = do { Parser.string "human";
               return (Human)
             }
          `Parser.orelse`
          do { Parser.string "ai";
               return (AI)
             }

data Kind = Corner | Tshape | Line deriving (Eq, Enum, Read, Show)
kind :: Parser.Parser Kind
kind =  do { Parser.string "corner";
             return (Corner)
           }
        `Parser.orelse`
        do { Parser.string "tshape";
             return (Tshape)
           }
        `Parser.orelse`
        do { Parser.string "line";
             return (Line)
           }

data Direction = North | East | South | West deriving (Eq, Enum, Read, Show)
direction :: Parser.Parser Direction
direction = do { Parser.string "north";
                 return (North)
               }
            `Parser.orelse`
            do { Parser.string "east";
                 return (East)
               }
            `Parser.orelse`
            do { Parser.string "south";
                 return (South)
               }
            `Parser.orelse`
            do { Parser.string "west";
                 return (West)
               }