open System
open System.IO

type MonkeyId = int

type WorryLevel = Int64

type Count = Int64

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
    { Setting: MonkeySetting
      Items: Map<WorryLevel, Count>
      ItemsMoved: Count }

type State = { Monkeys: Map<MonkeyId, Monkey> }

let split (separator: string) (input: string) = input.Split(separator)

let trimEnd (trimChar: char) (input: string) = input.TrimEnd(trimChar)

let parseMonkeyId line =
    line |> trimEnd ':' |> split " " |> Array.last |> int

let parseStartingItems line =
    line |> split ":" |> Array.last |> split "," |> Array.map int |> Array.toList

let parseOperation line =
    let parseExpression =
        function
        | "old" -> Old
        | value -> Value(int value)

    let formula = line |> split " = " |> Array.last

    match split " " formula with
    | [| x; "+"; y |] -> Add(parseExpression x, parseExpression y)
    | [| x; "*"; y |] -> Multiply(parseExpression x, parseExpression y)
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
        |> List.map (fun (worryLevel, count) -> (int64 worryLevel, int64 count))
        |> Map.ofList

    let monkey =
        { Setting = setting
          Items = items
          ItemsMoved = 0L }

    (monkeyId, monkey)

let parse newLine input =
    let monkeys =
        input
        |> split (String.replicate 2 newLine)
        |> Seq.map (parseMonkey newLine)
        |> Map.ofSeq

    { Monkeys = monkeys }

let evalOperation operation worryLevel =
    let evalExpression =
        function
        | Value x -> int64 x
        | Old -> worryLevel

    match operation with
    | Add (x, y) -> (evalExpression x) + (evalExpression y)
    | Multiply (x, y) -> (evalExpression x) * (evalExpression y)

let throw instruction worryLevel =
    if worryLevel % (int64 instruction.DivisableBy) = 0L then
        (instruction.TrueCase, worryLevel)
    else
        (instruction.FalseCase, worryLevel)

let bored x = x / 3L

let nextMonkey f setting =
    evalOperation setting.Operation
    >> f
    >> throw setting.Test

let removeWorryLevel worryLevel monkey =
    let count = Map.find worryLevel monkey.Items
    { monkey with
        Items = Map.remove worryLevel monkey.Items
        ItemsMoved = monkey.ItemsMoved + count }

let addWorryLevel worryLevel count monkey =
    let newCount =
        monkey.Items
        |> Map.tryFind worryLevel
        |> Option.map (fun c -> c + count)
        |> Option.defaultValue count

    let newItems = Map.add worryLevel newCount monkey.Items
    { monkey with Items = newItems }

let run f monkeyId monkeys =
    let monkey = Map.find monkeyId monkeys
    let newMonkeys =
        (monkeys, monkey.Items)
        ||> Map.fold (fun acc worryLevel count ->
            let (newMonkeyId, newWorryLevel) = nextMonkey f monkey.Setting worryLevel
            let newAcc =
                acc
                |> Map.change monkeyId (Option.map (removeWorryLevel worryLevel))
                |> Map.change newMonkeyId (Option.map (addWorryLevel newWorryLevel count))

            newAcc)

    newMonkeys

let flip f x y = f y x

let round f state =
    let newMonkeys =
        Map.keys state.Monkeys
        |> Seq.fold (flip (run f)) state.Monkeys

    { Monkeys = newMonkeys }

let part1 _ = bored

let part2 initialState x =
    let product =
        Map.values initialState.Monkeys
        |> Seq.map (fun m -> m.Setting.Test.DivisableBy)
        |> Seq.reduce ( * )
        |> int64

    x % product

let solve f newLine roundCount input =
    let initialState = parse newLine input
    let finalState =
        List.replicate roundCount (round (f initialState))
        |> List.fold (|>) initialState

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

solve part1 "\n" 20 example
solve part2 "\n" 10000 example

let input =
    Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllText

solve part1 Environment.NewLine 20 input
solve part2 Environment.NewLine 10000 input
