module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (an, ax, ru)
  = an

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (an, ax, ru)
  = ax

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (an, ax, ru)
  = ru


-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar c ((c', r):rs)
  | c == c'         = r
  | otherwise       = lookupChar c rs

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne "" _
  = ""
expandOne (c:cs) rus
  = (lookupChar c rus) ++ (expandOne cs rus)

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand ax num rus
  | num == 0        = ax
  | otherwise       = expand ax' (num-1) rus
    where ax'       = expandOne ax rus

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move cm an ((x, y), ori_an)
  | cm == 'F'       = ((x + cos an', y + sin an'), ori_an)
  | cm == 'L'       = ((x, y), ori_an + an) 
  | cm == 'R'       = ((x, y), ori_an - an)
    where an' = ori_an / 180 * pi


-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 cmds an co
  = traces
    where (traces, _)   = trace1' cmds an co ((0, 0), 90) []

          trace1' :: Commands -> Angle -> Colour -> TurtleState -> [ColouredLine] -> ([ColouredLine], Commands)
          trace1' "" _ _ _ tra
            = (tra, "")
          trace1' (c:cs) an co s tra
            | c == '['         = trace1' rest an co s (branch ++ tra)
            | c == ']'         = (tra, cs)
            | c == 'F'         = trace1' cs an co new_s (tra' : tra)
            | otherwise        = trace1' cs an co new_s tra

              where         
              new_s            = move c an s
              (new_p, _)       = new_s
              (ori_p, _)       = s
              tra'             = (ori_p, new_p, co)
              (branch, rest)   = trace1' cs an co s []


trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 cmd angle colour
  = trace2' cmd angle colour ((0, 0), 90) [((0, 0), 90)]

  where trace2' :: Commands -> Angle -> Colour -> TurtleState -> [TurtleState] -> [ColouredLine]
        trace2' "" _ _ _ _
          = []
        trace2' (c:cs) an co state stack
          | c == '['        = trace2' cs an co state (state : stack)
          | c == ']'        = trace2' cs an co ori_state stack
          | c == 'F'        = (ori_pos, new_pos, colour) : trace2' cs an co new_state stack
          | otherwise       = trace2' cs an co new_state stack

            where 
            new_state         = move c an state
            (ori_state : _)   = stack
            (new_pos, _)      = new_state
            (ori_pos, _)      = state



----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
