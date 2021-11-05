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
--Functions for working with systems.
-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (an, ax, ru)
  = an

axiom :: LSystem -> String
axiom (an, ax, ru)
  = ax

rules :: LSystem -> Rules
rules (an, ax, ru)
  = ru

lookupChar :: Char -> Rules -> String
lookupChar c ((c', r):rs)
  | c == c'         = r
  | otherwise       = lookupChar c rs

expandOne :: String -> Rules -> String
expandOne "" _
  = ""
expandOne (c:cs) rus
  = (lookupChar c rus) ++ (expandOne cs rus)

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

bush :: LSystem
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

result :: LSystem
result 
  = (30,
     "M--M--M--M--M--M",
     [('M', "M[+M][++M]M"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]

    )