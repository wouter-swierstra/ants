module Test
  ( instructionLimit
  , pointerIsValid
  , checkInstructions
  , allInstructionsValid
  ) where

import Parser.StateMachineParser
import Map
import Data.Either
import Control.Monad.State
import Data.Array

-- Module for dynamic tests on parsed structures

--------------------------------------------------------------------------------
-- StateMachine tests

type StateMachineChecker = StateT Int (Either String) ()

-- | Amount of instructions in a state machine cannot exceed 10000
instructionLimit :: StateMachine -> Bool
instructionLimit arr = length arr <= 10000

-- | Check that a pointer is valid in the current context (array)
pointerIsValid :: Pointer -> StateMachine -> Bool
pointerIsValid ptr arr = 0 <= ptr && ptr < length arr

-- | Dynamically checks that all the instructions are valid. Which means that
-- they
--
-- * Do not have pointers that point to nonexistent states (0 <= st <= length arr)
-- * No markers have a label that may not exist (0 <= i <= 5)
-- * Flip is only used on natural numbers (0 < p)
--
-- FIXME: this function should point out where the error occurred (line number
-- with the invalid state), an additional function can then be defined for
-- simply `checking for errors'
--checkInstructions :: StateMachine -> a --StateT StateMachine (ErrorT String)
checkInstructions arr =
  let stateVal (LSense _ ptr1 ptr2 c) = and [ptrIsVal ptr1, ptrIsVal ptr2, condIsVal c]
      stateVal (LMark i ptr )         = and [iIsVal i, ptrIsVal ptr]
      stateVal (LUnmark i ptr)        = and [iIsVal i, ptrIsVal ptr]
      stateVal (LPickup ptr1 ptr2)    = and [ptrIsVal ptr1, ptrIsVal ptr2]
      stateVal (LDrop ptr)            = ptrIsVal ptr
      stateVal (LTurn _ ptr)          = ptrIsVal ptr
      stateVal (LMove ptr1 ptr2)      = and [ptrIsVal ptr1, ptrIsVal ptr2]
      stateVal (LFlip p ptr1 ptr2)    = and [pIsVal p, ptrIsVal ptr1, ptrIsVal ptr2]

      -- Predicates on values
      ptrIsVal  x           = 0 <= x && x < length arr
      iIsVal    x           = 0 <= x && x <= 6
      pIsVal    x           = 0 < x
      condIsVal (CMarker i) = iIsVal i
      condIsVal _           = True

      -- Checker
      checker :: StateMachineChecker
      checker =
        do i <- get
           if i < length arr
           then if stateVal (arr ! i)
                then do put (i+1)
                        checker
                else fail $ "checkInstructionState dynamically found an error on line " ++ show i ++ "."
           else return ()
  in runStateT checker 0

-- | Bool version of the above, does not give details on the location of the
-- error
allInstructionsValid :: StateMachine -> Bool
allInstructionsValid = isRight . checkInstructions

--------------------------------------------------------------------------------
-- Map tests


