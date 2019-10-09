module PenaliseStrategy
open Prisoner.Domain


type State = 
  { Previous : (Action*Action) list }

let private memorySize = 10
let initial = 
  { Previous = [] }

let play state = 
  if List.isEmpty state.Previous then 
    Collaborate
  elif state.Previous |> List.exists(fun (opponent, my) -> opponent = Betray) then
    Betray
  else Collaborate

let learn state your opponent =
  { Previous = (opponent,your) :: state.Previous |> List.truncate memorySize}

let register() =
  register "Tuomas - Simple Penalise1 strategy" initial play learn
