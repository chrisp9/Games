type gameState =
   | Success
   | Fail
   | InProgress of int

let printState gameState =
   match gameState with 
   | Success -> printfn "You won!"
   | Fail -> printfn "You lost!"
   | _ -> ()

let initialGuess (word : char list) =
   word |> List.map (fun _ -> '-') |> Array.ofList |> System.String
   
let rec attemptGuess (word : char list, current : char list, guess : char) =
   let unveiled = 
      match word.Head with
      | x when guess = word.Head -> word.Head.ToString()
      | _ -> current.Head.ToString()

   match word.Tail.Length with
   | 0 -> unveiled
   | _ -> unveiled + attemptGuess(word.Tail, current.Tail, guess)

let advanceState state guessWasCorrect (currentWord : string) =
   match state, guessWasCorrect, currentWord.Contains("-") with
   | InProgress x, true, true when x < 8 -> gameState.InProgress(x)
   | InProgress x, false, true when x < 8 -> gameState.InProgress(x+1)
   | InProgress x, false, true when x >= 8 -> gameState.Fail
   | _, true, false -> gameState.Success
   | _, _, _ -> gameState.Fail

let word() =
   let dict = ["hello"; "world"; "welcome"; "home";"antivivisectionist"]
   dict.Item(System.Random().Next(dict.Length))

let rec gameLoop(word, current, (state : gameState)) =
   let guess = System.Console.ReadKey().Key.ToString().ToLower().ToCharArray() |> List.ofArray |> List.head
   let newCurrent = attemptGuess(word, current, guess)

   let wasCorrect = current <> (newCurrent.ToCharArray() |> List.ofArray)
   match wasCorrect with 
   | true -> System.Console.WriteLine("Guessed correctly!")
   | false -> System.Console.WriteLine("Incorrect guess!")

   System.Console.WriteLine("\n" + newCurrent)
   let newState = advanceState (state) (wasCorrect) newCurrent

   printState(newState)

   match newState with 
   | InProgress _ -> gameLoop(word, newCurrent.ToCharArray() |> List.ofArray, newState)
   | _ -> ()
   ()

[<EntryPoint>]
let main argv = 
   while true do
      let theWord = word()
      let initialGuess = initialGuess(Seq.toList (theWord.ToCharArray()))
      System.Console.WriteLine (initialGuess)
      gameLoop(theWord.ToCharArray() |> List.ofArray, initialGuess.ToCharArray() |> List.ofArray, gameState.InProgress(0))
   
   printfn "%A" argv
   0 // return an integer exit code