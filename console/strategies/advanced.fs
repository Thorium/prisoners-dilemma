module AdvancedStrategy
open Prisoner.Domain
let myname = "Tuomas - Advanced1 strategy"
let mutable private opponentIdx = 0
type State = 
  { Previous : (Action*Action) list
    Opponent : Strategy }

let initial = { Previous = []; Opponent = Unchecked.defaultof<Strategy> }
let rnd = new System.Random()

// These are quick & dirty hacks where I try to avoid
// "I won't tell my move before you tell yours" - stackoverflows if playing against self.
let mutable private detectRecursion = 0
let mutable private tempOpponet = Unchecked.defaultof<Strategy>
let mutable private skipWhenPlayAginstSelf = 0

let play state = 

  let mypos, myStrategy = 
    // This only works when strategies are public and not random order.
    strategies |> Seq.mapi (fun idx s1 -> 
                if s1.Name = myname then Some (idx, s1) else None) |> Seq.pick(fun x -> x)

  let opponent = 
      if List.isEmpty state.Previous then
          let opponent = // Identify current opponent
              if opponentIdx < mypos then
                    strategies.[opponentIdx % strategies.Count]
              elif opponentIdx >= mypos && opponentIdx < mypos + strategies.Count then
                    strategies.[opponentIdx - mypos]
              else
                    strategies.[opponentIdx % strategies.Count]

          //System.Console.WriteLine ("opponent: "+opponentIdx.ToString())
          //if opponentIdx = mypos then
          //    System.Console.WriteLine ("Its me!")
          System.Console.WriteLine ("I think it's " + opponent.Name)
          // ugly fix, as I was running out of time:
          if opponent.Name = myname then 
            if skipWhenPlayAginstSelf = 0 then
              skipWhenPlayAginstSelf <- 1
              opponentIdx <- opponentIdx - 1 
            else
              skipWhenPlayAginstSelf <- 0
          opponentIdx <- opponentIdx + 1 

          opponent
      else state.Opponent
  tempOpponet <- opponent
  if detectRecursion > 20 || opponent.Name = myname then
      // Playing against self. Depends on point calculation style: should I play 200:200 or 300:0 ?
      Collaborate
  else
  let rec simulateTurns (moveHistory :(Action*Action) list) oppState =
    match moveHistory with
    | [] -> //Initial move. Simulate opponent's initiative. Must try multiple times as it could be random.
        let initState =  [1 .. 100] |> List.map(fun _ -> opponent.Play opponent.Initial)
        
        // Todo: write initial strategy
        if initState |> Seq.forall(fun a -> a = Collaborate) then
            Collaborate
        else Betray
        
    | (opp,my)::t -> // Move n+1. Have to recursively train the opponent to query the move.
        let newState = opponent.Learn oppState opp my        
        match t with
        | [] ->
            let nextMove = 
                [1 .. 100] |> List.map(fun _ -> opponent.Play newState)
        
            if nextMove |> Seq.forall(fun a -> a = Collaborate) then 
                // Todo: write more agressive Betray strategy, something like:
                //if state.Previous.Length > 195 && rnd.NextDouble() > 0.7 then Betray 
                //else 
                    Collaborate
            else Betray

        | x -> simulateTurns x newState
  try
      detectRecursion <- detectRecursion + 1
      let myAct = simulateTurns (state.Previous |> List.rev) opponent.Initial
      if detectRecursion > 20 then Collaborate 
      else
      detectRecursion <- 0
      myAct
  with // If opponent causes error
  | err -> 
      System.Console.WriteLine err.Message
      detectRecursion <- 0
      Betray

let learn state your opponent =
  { Opponent = tempOpponet; Previous = (opponent,your) :: state.Previous}

let register() =
  register myname initial play learn