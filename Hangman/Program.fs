// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type gameState =
   | Success
   | Fail
   | InProgress of int

let printState gameState =
   match gameState with 
   | Success -> printfn "You won!"
   | Fail -> printfn "You lost!"
   | InProgress _ -> printfn "Incorrect guess"

let initialGuess (word : char list) =
   word |> List.map (fun _ -> '-') |> Array.ofList |> System.String
   
let rec attemptGuess (word : char list, current : char list, guess : char) =
   let unveiled = 
      match word.Head with
      | x when guess = word.Head -> word.Head.ToString()
      | _ -> current.Head.ToString()

   match word.Tail.Length with
   | 0 -> unveiled
   | _ -> unveiled + attemptGuess(word.Tail, word.Tail, guess)

let advanceState state =
   match state with
   | InProgress x when x < 9 -> gameState.InProgress(x + 1)
   | InProgress x when x >= 9 -> gameState.Fail
   | _ -> gameState.Success

let word() =
   let dict = ["hello"; "world"]
   List.sortBy (fun _ -> System.Random().Next(dict.Length)) dict |> List.head

let rec gameLoop(word, current) =
   let guess = System.Console.ReadKey().Key.ToString().ToLower().ToCharArray() |> List.ofArray |> List.head
   let newCurrent = attemptGuess(word, current, guess)
   System.Console.WriteLine(newCurrent)
   gameLoop(word, newCurrent.ToCharArray() |> List.ofArray)

[<EntryPoint>]
let main argv = 
   let theWord = word()
   let initialGuess = initialGuess(Seq.toList (theWord.ToCharArray()))
   System.Console.WriteLine (initialGuess)
   gameLoop(theWord.ToCharArray() |> List.ofArray, initialGuess.ToCharArray() |> List.ofArray)
   printfn "%A" argv
   0 // return an integer exit code