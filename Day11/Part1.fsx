open System
open System.IO

type MonkeyId = int

type WorryLevel = int

type Expression =
    | Value of int
    | Old

type Operation =
    | Add of Expression * Expression
    | Multiply of Expression * Expression

type ThrowInstruction =
    { DivisableBy: int
      TrueCase: MonkeyId
      FalseCase: MonkeyId }

type MonkeySetting =
    { Operation: Operation
      Test: ThrowInstruction }

type Monkey =
    { Setting : MonkeySetting
      Items : Map<WorryLevel, Int64>
      ItemsMoved : Int64 }

type State = { Monkeys: Map<MonkeyId, Monkey> }

let split (separator: string) (input: string) = input.Split(separator)

let trimEnd (trimChar: char) (input: string) = input.TrimEnd(trimChar)

let trim (trimChar: char) (input: string) = input.Trim(trimChar)

let parseMonkeyId line =
    let [| _; id |] = line |> trimEnd ':' |> split " "
    int id

let parseStartingItems line =
    let startingItems =
        line
        |> split ":"
        |> Array.last
        |> split ","
        |> Array.map (trim ' ' >> int)

    List.ofArray startingItems

let parseOperation line =
    let parseExpression = function
        | "old" -> Old
        | value -> Value(int value)

    let formula = line |> split " = " |> Array.last

    match split " " formula with
    | [| x; "+"; y |] -> Add (parseExpression x, parseExpression y)
    | [| x; "*"; y |] -> Multiply (parseExpression x, parseExpression y)
    | _ -> invalidArg (nameof line) line

let parseThrowInstruction [| line1; line2; line3 |] =
    let divisableBy = line1 |> split " " |> Array.last |> int
    let trueCase = line2 |> split " " |> Array.last |> int
    let falseCase = line3 |> split " " |> Array.last |> int

    { DivisableBy = divisableBy
      TrueCase = trueCase
      FalseCase = falseCase }

let parseMonkey newLine part =
    let lines = split newLine part
    let monkeyId = parseMonkeyId lines[0]
    let startingItems = parseStartingItems lines[1]
    let setting =
        { Operation = parseOperation lines[2]
          Test = parseThrowInstruction lines[3..] }

    let items =
        startingItems
        |> List.countBy id
        |> List.map (fun (worryLevel, count) -> (worryLevel, int64 count))
        |> Map.ofList

    let monkey = { Setting = setting; Items = items; ItemsMoved = 0L }
    (monkeyId, monkey)

let parse newLine input =
    let monkeys =
        input
        |> split (String.replicate 2 newLine)
        |> Seq.map (parseMonkey newLine)
        |> Map.ofSeq

    { Monkeys = monkeys }

let evalOperation operation value =
    let evalExpression =
        function
        | Value x -> x
        | Old -> value

    match operation with
    | Add (x, y) -> (evalExpression x) + (evalExpression y)
    | Multiply (x, y) -> (evalExpression x) * (evalExpression y)

let throw instruction value =
    if value % instruction.DivisableBy = 0 then
        (instruction.TrueCase, value)
    else
        (instruction.FalseCase, value)

let bored x = x / 3

let nextMonkey setting =
    evalOperation setting.Operation
    >> bored
    >> throw setting.Test

let run monkeyId monkeys =
    let monkey =  Map.find monkeyId monkeys
    let newMonkeys =
        (monkeys, monkey.Items)
        ||> Map.fold (fun acc worryLevel count ->
            let monkey = Map.find monkeyId monkeys
            let (newMonkeyId, newWorryLevel) = nextMonkey monkey.Setting worryLevel
            let newAcc =
                acc
                |> Map.change
                    monkeyId
                    (Option.map (fun m ->
                        { m with
                            Items = Map.remove worryLevel m.Items
                            ItemsMoved = m.ItemsMoved + count }))
                |> Map.change
                    newMonkeyId
                    (Option.map (fun m ->
                        let newCount =
                            m.Items
                            |> Map.tryFind newWorryLevel
                            |> Option.map (fun c -> c + count)
                            |> Option.defaultValue count

                        let newItems = Map.add newWorryLevel newCount m.Items
                        { m with Items = newItems }))
                    
            newAcc)

    newMonkeys

let round state =
    let flip f x y = f y x
    let newMonkeys =
        Map.keys state.Monkeys
        |> Seq.sort
        |> Seq.fold (flip run) state.Monkeys

    { Monkeys = newMonkeys }

let solve newLine roundCount input =
    let initialState = parse newLine input
    let finalState = 
        List.replicate roundCount round
        |> List.fold (fun state f -> f state) initialState

    let monkeyBusinessLevel =
        finalState.Monkeys
        |> Map.values
        |> Seq.map (fun m -> m.ItemsMoved)
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.reduce ( * )
    
    monkeyBusinessLevel

let example =
    @"Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"

parse "\n" example |> fun state -> state.Monkeys |> run 0

solve "\n" 1 example
solve "\n" 20 example


let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve Environment.NewLine 20 input
solve Environment.NewLine 10000 input