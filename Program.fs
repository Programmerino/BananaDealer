module BananaDealer

open FSharp.Control
open FSharp.Control.Reactive
open System
open FSharpPlus
open System.Threading.Tasks
open FSharp.Data
open Connected
open FSharp.Control.Reactive.Builders

type Letter = char

module List =
    let removeFirst predicate list =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | h :: t when predicate h -> (List.rev acc) @ t
            | h :: t -> loop (h :: acc) t

        loop [] list

// module Letter =
//     let fromChars xs = xs |> map (Letter)
//     let fromStringMatrix (xs: string[][]): Letter[][] = xs |> map(map(fun x -> Letter x[0]))
//     let toChars xs = xs |> map (fun (Letter (x)) -> x)
//     let toStrings = toChars >> map string >> toArray

module Shuffle =
    let shuffle (array: 'a []) =
        let swap i j =
            let temp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- temp

        let random = new System.Random()
        let len = array.Length

        [ 0 .. len - 2 ]
        |> Seq.iter (fun i -> swap i (random.Next(i, len)))

        array

    let insert xs v =
        let i = Random().Next(List.length xs - 1)
        xs[0 .. i - 1] ++ [ v ] ++ xs[i..]

type PlayerState =
    { PersonalPile: Letter list
      Matrix: (Letter option) array array }

module PlayerState =
    let initPile pile = { PersonalPile = pile; Matrix = [||] }

type GameState =
    { PickPile: Letter list
      Players: PlayerState array }

module GameState =
    let letter n x =
        Array.replicate n (String.toArray x)
        |> Array.concat

    // 2: J, K, Q, X, Z
    // 3: B, C, F, H, M, P, V, W, Y
    // 4: G
    // 5: L
    // 6: D, S, U
    // 8: N
    // 9: T, R
    // 11: O
    // 12: I
    // 13: A
    // 18: E
    let initialPick =
        [||]
        ++ letter 2 "JKQXZ"
        ++ letter 3 "BCFHMPVWY"
        ++ letter 4 "G"
        ++ letter 5 "L"
        ++ letter 6 "DSU"
        ++ letter 8 "N"
        ++ letter 9 "TR"
        ++ letter 11 "O"
        ++ letter 12 "I"
        ++ letter 13 "A"
        ++ letter 18 "E"

    let PIECE_COUNT = 21

    // FIXME: Vary starting amount based on player count
    let generate n =
        let pile =
            Shuffle.shuffle (Array.copy initialPick)
            |> Array.toList

        let players =
            [| 1..n |]
            |> Array.map (fun i ->
                let first = (i - 1) * PIECE_COUNT
                let last = i * PIECE_COUNT
                PlayerState.initPile pile[first..last])

        let pickPile = pile[n * PIECE_COUNT + 1 ..] in

        { PickPile = pickPile
          Players = players }

open GameState
open System.Threading
open System
open Websocket.Client
open System.IO


type Game =
    | Awaiting of int
    | Started of GameState
    | Finished

let gameState x =
    match x with
    | Awaiting n -> $"awaiting {n} players before continuing"
    | Started _ -> "in progress"
    | Finished -> "over"


let words =
    File.ReadAllLines("dict.txt")
    |> Array.skip 2
    |> Set.ofArray

// Execution format: bot.exe 0 192.168.1.23
// 0 is the player ID of that process, 192.168.1.23 is the IP address of the WebSocket server

module Messaging =
    type Message =
        JsonProvider<""" { "type": "Message Type (e.g. Start)", "from": -1, "to": -1, "msg": "msgJson" } """, InferTypesFromValues=true> // from refers to the player sending the message. -1 indicates the dealer sent it. to likewise, when it doesn't make sense for the message, is -1.

    type Ready = JsonProvider<""" { "name": "Bot name" } """, InferTypesFromValues=true> // Sent to indicate a player is ready for the game to begin. They can provide a name it should be referred to with.
    type Info = JsonProvider<""" { "message": "Message here" } """, InferTypesFromValues=true> // Generic string message to be printed by the dealer, or for informal communication between players.
    type Start = JsonProvider<""" { "piles": [["a", "b", "..."], ["c", "d", "..."]] } """, InferTypesFromValues=true> // Only sent by the dealer, array of the pile given to each player
    type Complete = JsonProvider<""" { "matrix": [["a", "b", "..."]] } """, InferTypesFromValues=true> // Sent by players, allegedly complete matrix when they want to pick a new piece, or if they believe they have won the game.
    type Trade = JsonProvider<""" { "piece": "a" } """, InferTypesFromValues=true> // Sent by players, piece wanted to be exchanged. The dealer will not reply if there aren't enough pieces.
    type PickGive = JsonProvider<""" { "pieces": ["a", "b"] } """, InferTypesFromValues=true> // Sent by the dealer after Complete if the game is still going. Players should index into the array by their player index to find their letter.
    type TradeGive = JsonProvider<""" { "pieces": ["a", "b", "c"] } """, InferTypesFromValues=true> // Sent by the dealer on Trade to a specific player. Will always contain three letters.
    type End = JsonProvider<""" { "reason": "Reason here" } """, InferTypesFromValues=true> // Sent by players or the dealer to indicate the game should end (someone won, due to an exception, etc.).

    type InnerMessage =
        | ReadyMsg of Ready.Root
        | InfoMsg of Info.Root
        | StartMsg of Start.Root
        | CompleteMsg of Complete.Root
        | TradeMsg of Trade.Root
        | PickGiveMsg of PickGive.Root
        | TradeGiveMsg of TradeGive.Root
        | EndMsg of End.Root

    type From =
        | Dealer
        | Player of int

    type TypedMessage =
        { from: From
          ``to``: int option
          message: InnerMessage }

    module InnerMessage =
        let typeString x =
            match x with
            | ReadyMsg _ -> "Ready"
            | InfoMsg _ -> "Info"
            | StartMsg _ -> "Start"
            | CompleteMsg _ -> "Complete"
            | TradeMsg _ -> "Trade"
            | PickGiveMsg _ -> "PickGive"
            | TradeGiveMsg _ -> "TradeGive"
            | EndMsg _ -> "End"

        let toJson (x: InnerMessage) =
            match x with
            | ReadyMsg x -> x.JsonValue.ToString()
            | InfoMsg x -> x.JsonValue.ToString()
            | StartMsg x -> x.JsonValue.ToString()
            | CompleteMsg x -> x.JsonValue.ToString()
            | TradeMsg x -> x.JsonValue.ToString()
            | PickGiveMsg x -> x.JsonValue.ToString()
            | TradeGiveMsg x -> x.JsonValue.ToString()
            | EndMsg x -> x.JsonValue.ToString()

    module TypedMessage =
        let toJson x =
            let type_ = InnerMessage.typeString x.message

            let from =
                match x.from with
                | Dealer -> -1
                | Player x -> x

            let to_ = x.``to`` |> Option.defaultValue (-1)
            let msg = InnerMessage.toJson x.message

            Message
                .Root(type_, from, to_, msg)
                .JsonValue.ToString()

    type State =
        { Messages: Message.Root list
          Game: Game }


    let printMatrix (rows: (Letter option) [] []) =
        printfn ""

        rows
        |> Array.iter (fun x ->
            Array.iter
                (fun x ->
                    let x = Option.defaultValue ' ' x
                    printf $"{x}")
                x

            printfn "")

    let ofStringMatrix =
        Array.map (Array.map (fun x -> if x = "" then None else Some(char x)))

    let checkMatrix (player: PlayerState) (rows: (Letter option) [] []) =

        // TODO: More historical checks
        let bigger = Array.length player.Matrix < Array.length rows

        // Valid board
        let cols = Array.transpose rows

        let sub =
            rows ++ cols
            |> Array.map (Array.map (Option.defaultValue ' '))
            |> Array.map (string)
            |> Array.collect (
                String.split [ " " ]
                >> Seq.filter (fun x -> x <> "")
                >> Seq.toArray
            )
            |> Set.ofArray

        let validWords = Set.isSubset sub words

        let connected =
            Kosaraju()
                .IsConnected(
                    array2D (
                        rows
                        |> Array.map (Array.map (fun x -> if x = None then 0 else 1))
                    )
                )

        if validWords && connected then
            ()
        else
            failwith
                $"Illegal matrix detected: {printMatrix rows}. The following should be true: ValidWords: {validWords}, Connected: {connected}"

    let checkTrade (player: PlayerState) letter =
        let possible =
            Array.concat (player.Matrix |> Array.map (Array.choose id))
            ++ (Array.ofList player.PersonalPile)
            |> Set.ofArray

        let usedGiven = possible |> Set.contains letter

        if usedGiven then
            ()
        else
            failwith $"Illegal trade detected: {letter}. The following should be true: UsedGiven: {usedGiven}"

    let update audit emit (state: State) (omsg: Message.Root) =
        let msg =
            match omsg.Type with
            | "Ready" -> ReadyMsg(Ready.Parse(omsg.Msg))
            | "Info" -> InfoMsg(Info.Parse(omsg.Msg))
            | "Start" -> StartMsg(Start.Parse(omsg.Msg))
            | "Complete" -> CompleteMsg(Complete.Parse(omsg.Msg))
            | "Trade" -> TradeMsg(Trade.Parse(omsg.Msg))
            | "PickGive" -> PickGiveMsg(PickGive.Parse(omsg.Msg))
            | "TradeGive" -> TradeGiveMsg(TradeGive.Parse(omsg.Msg))
            | "End" -> EndMsg(End.Parse(omsg.Msg))
            | _ -> failwith $"Unexpected message type {omsg.Type}"

        let from = omsg.From
        let to_ = omsg.To

        let wrongMessageType () =
            printfn
                $"Player {from} sent a message of type {omsg.Type} which is invalid while the game is {gameState state.Game}"

            exit 1

        let game =
            match msg with
            | InfoMsg x ->
                printfn $"{x}"
                state.Game
            | EndMsg x ->
                printfn $"Player {from} forfeits: {x}"
                exit 0
            | _ ->
                match state.Game with
                | Awaiting n ->
                    match msg with
                    | ReadyMsg x ->
                        printfn $"{x} ({from}) is ready. Waiting for {n - 1} more..."

                        if n - 1 = 0 then
                            let game = generate n

                            emit (
                                { from = Dealer
                                  ``to`` = None
                                  message =
                                    StartMsg(
                                        Start.Root(
                                            game.Players
                                            |> Array.map (fun x ->
                                                x.PersonalPile
                                                |> List.toArray
                                                |> Array.map (string))
                                        )
                                    ) }
                            )

                            Started(game)
                        else
                            Awaiting(n - 1)
                    | _ -> wrongMessageType ()
                | Started game ->
                    let n = length game.Players

                    match msg with
                    | CompleteMsg x ->
                        let matrix = ofStringMatrix x.Matrix

                        if audit then
                            checkMatrix game.Players[from] matrix

                        let didWin = length game.PickPile <= n

                        let players = Array.copy game.Players
                        players[from] <- { players[from] with Matrix = matrix }

                        if didWin then
                            emit (
                                { from = Dealer
                                  ``to`` = None
                                  message = EndMsg(End.Root($"Player {from} won!")) }
                            )

                            Finished
                        else
                            let picked = game.PickPile[0 .. n - 1]

                            players
                            |> Array.iteri (fun i _ ->
                                players[i] <- { players[i] with PersonalPile = picked[i] :: players[0].PersonalPile })

                            let game =
                                { game with
                                    PickPile = game.PickPile[n..]
                                    Players = players }

                            emit (
                                { from = Dealer
                                  ``to`` = None
                                  message = PickGiveMsg(PickGive.Root(picked |> List.toArray |> Array.map (string))) }
                            )

                            Started(game)
                    | TradeMsg x ->
                        let piece = char x.Piece

                        if audit then
                            checkTrade game.Players[from] piece

                        if List.length game.PickPile >= 3 then
                            let pickPile = Shuffle.insert game.PickPile piece
                            let picked = pickPile[0..2]
                            let players = Array.copy game.Players

                            players[from] <- { players[from] with
                                PersonalPile =
                                    (players[from].PersonalPile
                                     |> List.removeFirst (fun y -> y = piece))
                                    ++ picked }

                            let game =
                                { game with
                                    PickPile = game.PickPile[3..]
                                    Players = players }

                            emit (
                                { from = Dealer
                                  ``to`` = Some(from)
                                  message = TradeGiveMsg(TradeGive.Root(picked |> List.toArray |> Array.map (string))) }
                            )

                            Started(game)
                        else
                            Started(game)
                    | _ -> wrongMessageType ()
                | Finished -> wrongMessageType ()

        { state with
            Messages = omsg :: state.Messages
            Game = game }


    let start n =
        let exitEvent = new ManualResetEvent(false)

        observe {
            let url = new Uri("ws://127.0.0.1:8080")
            use client = new WebsocketClient(url)

            client.DisconnectionHappened
            |> Observable.iter (fun x -> failwith $"{x}")
            |> ignore

            do!
                client.StartOrFail()
                |> Async.AwaitTask
                |> Observable.ofAsync

            printfn "Server started!"

            let initialState = { Messages = []; Game = Awaiting n }

            let myFold audit =
                (update audit (fun msg -> client.Send(TypedMessage.toJson msg)))

            let history =
                client.MessageReceived
                |> Observable.map (fun x -> Message.Parse(x.Text))
                |> Observable.scanInit initialState (myFold false)
                |> Observable.takeWhile (fun x -> x.Game <> Finished)
                |> Observable.wait
                |> (fun x -> x.Messages)

            // Replay messages with audit
            history
            |> List.fold (myFold true) initialState
            |> ignore

            yield ()
        }

module Command =
    open System.Diagnostics

    type CommandResult =
        { StandardOutput: string
          StandardError: string }

    let executeCommand executable args =
        async {
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- executable

            for a in args do
                startInfo.ArgumentList.Add(a)

            startInfo.RedirectStandardOutput <- true
            startInfo.RedirectStandardError <- true
            startInfo.UseShellExecute <- false
            startInfo.CreateNoWindow <- true
            use p = new Process()
            p.StartInfo <- startInfo
            p.Start() |> ignore

            let outTask =
                Task.WhenAll(
                    [| p.StandardOutput.ReadToEndAsync()
                       p.StandardError.ReadToEndAsync() |]
                )

            do! p.WaitForExitAsync() |> Async.AwaitTask
            let! out = outTask |> Async.AwaitTask
            printfn "%s" out[0]
            printfn "%s" out[1]

            if p.ExitCode <> 0 then
                failwith "Non-zero exit code"

            return
                { StandardOutput = out[0]
                  StandardError = out[1] }
        }

    let executeShellCommand command =
        executeCommand "/usr/bin/env" [ "-S"; "bash"; "-c"; command ]

open Command
// websocat -s 8080
[<EntryPoint>]
let main n =
    observe {
        let! x = Messaging.start (int n[0])
        printfn "%A" x
        yield 0
    }
    |> Observable.wait
