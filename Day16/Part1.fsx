open System
open System.IO

type ValveId = ValveId of string

type FlowRate = int

type Minute = int

type Valves = private Valves of Map<ValveId, FlowRate>

type Tunnels = private Tunnels of Map<ValveId, list<ValveId>>

let split (separator: string) (input: string) = input.Split(separator)

let parseValveIds input =
    match split "," input with
    | [| p1 |] -> p1 |> split " " |> Array.last |> ValveId |> List.singleton
    | parts ->
        let head = parts[0] |> split " " |> Array.last |> ValveId

        let tail =
            parts[1..]
            |> Array.map (fun str -> str.Trim() |> ValveId)
            |> Array.toList

        head :: tail

let parseValve input =
    let parts = split " " input
    let id = parts[1]
    let flowRate = parts[4] |> split "=" |> Array.last |> int
    (ValveId id, flowRate)

let parseLine (valves, tunnels) line =
    let [| part1; part2 |] = split ";" line
    let valveId, flowRate = parseValve part1
    let nextValveIds = parseValveIds part2
    (Map.add valveId flowRate valves, Map.add valveId nextValveIds tunnels)

let parse lines =
    let valves, tunnels =
        ((Map.empty, Map.empty), lines) ||> Seq.fold parseLine

    (Valves valves, Tunnels tunnels)
    
let minutes (Tunnels tunnels) v1 v2 =
    let rec imp minutesSoFar visited v =
        if v = v2 then
            [ (minutesSoFar + 1) ]
        else
            Map.find v tunnels
            |> List.except visited
            |> List.collect (imp (minutesSoFar + 1) (v :: visited))

    tunnels
    |> Map.find v1
    |> List.collect (imp 1 [ v1 ])
    |> List.min

let allPaths tunnels (Valves valves) =
    let flowRateValves =
        valves
        |> Map.filter (fun valveId flowRate -> flowRate <> 0 || valveId = ValveId "AA")
        |> Map.keys
        |> Seq.toList

    flowRateValves
    |> List.collect (fun v1 ->
        flowRateValves
        |> List.choose (fun v2 ->
            if v1 <> v2 then
                Some (v1, v2)
            else
                None))
    |> List.fold (fun acc (v1, v2) ->
        let minutes =
            match Map.tryFind (v2, v1) acc with
            | None -> minutes tunnels v1 v2
            | Some x -> x

        Map.add (v1, v2) minutes acc) Map.empty

type Step =
    { Current: ValveId
      Visited: list<ValveId>
      TotalMinutes: Minute
      LastPressure: int
      TotalPressure: int }

let adjust minutes step =
    let diff = minutes - step.TotalMinutes
    { step with
        TotalMinutes = minutes
        TotalPressure = step.TotalPressure + (diff * step.LastPressure)}

let findPaths paths (Valves valves) =
    let rec imp maxStep steps =
        match steps with
        | [] -> maxStep
        | s :: ss ->
            let nexts =
                paths
                |> Map.filter (fun (v1, v2) _ -> v1 = s.Current && not (List.contains v2 s.Visited))
                |> Map.toList
                |> List.map (fun ((_, v2), minutes) ->
                    let newPressure = Map.find v2 valves
                    { Current = v2
                      Visited = v2 :: s.Visited
                      TotalMinutes = s.TotalMinutes + minutes
                      LastPressure = s.LastPressure + newPressure
                      TotalPressure = s.TotalPressure + (s.LastPressure * minutes) + newPressure })

            if List.isEmpty nexts then
                let newStep = adjust 30 s
                let newMaxStep =
                    if newStep.TotalPressure > maxStep.TotalPressure then
                        newStep
                    else maxStep

                imp newMaxStep ss
            else
                let unfinished, finished =
                    nexts
                    |> List.partition (fun s -> s.TotalMinutes < 30)
                
                let newMaxStep = imp maxStep unfinished
                let newStep =
                    (newMaxStep, finished)
                    ||> List.fold (fun acc step ->
                        let adjusted = adjust 30 step
                        if adjusted.TotalPressure > acc.TotalPressure then
                            adjusted
                        else
                            acc) 

                imp newStep ss

    let initialStep =
        { Current = ValveId "AA"
          Visited = [ ValveId "AA"]
          TotalMinutes = 1
          LastPressure = 0
          TotalPressure = 0 }

    imp initialStep [ initialStep ]

let solve lines =
    let valves, tunnels = parse lines
    let paths = allPaths tunnels valves
    let maxStep = findPaths paths valves
    maxStep.TotalPressure

let example =
    @"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"

split "\n" example |> solve

let lines =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines

solve lines