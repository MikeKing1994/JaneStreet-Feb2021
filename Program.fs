type LetterState = 
    | Wrong
    | Close // right letter, wrong space
    | Correct

type Word = 
    {
        Letter1: char
        Letter2: char
        Letter3: char
        Letter4: char
        Letter5: char
    }

module Word = 
    let create (word: string) = 
        {
            Letter1 = word.[0]
            Letter2 = word.[1]
            Letter3 = word.[2]
            Letter4 = word.[3]
            Letter5 = word.[4]
        }

type GuessedWord = 
    {
        Index: int
        Word: Word
        Letter1State: LetterState
        Letter2State: LetterState
        Letter3State: LetterState
        Letter4State: LetterState
        Letter5State: LetterState
    }

module GuessedWord = 
    let isCorrect gWord = 
        gWord.Letter1State = Correct
        && gWord.Letter2State = Correct
        && gWord.Letter3State = Correct
        && gWord.Letter4State = Correct
        && gWord.Letter5State = Correct

type Wordle = 
    {
        Answer: Word
        Guesses: GuessedWord array
        IsOver: bool
    }

module Wordle = 
    let createNewGame answer = 
        {
            Answer = answer
            Guesses = [||]
            IsOver = false
        }

let checkGuessStatus 
    answer 
    guess 
    letterAccessor 
    wrongLetterAccessor1
    wrongLetterAccessor2
    wrongLetterAccessor3
    wrongLetterAccessor4 = 
    if letterAccessor answer =  letterAccessor guess then Correct
    else if wrongLetterAccessor1 answer = letterAccessor guess then Close
    else if wrongLetterAccessor2 answer = letterAccessor guess then Close
    else if wrongLetterAccessor3 answer = letterAccessor guess then Close
    else if wrongLetterAccessor4 answer = letterAccessor guess then Close
    else Wrong

let guess answer index (guess: string) = 
    let guessedWord = Word.create guess
    {
        Index = index
        Word = guessedWord
        Letter1State = 
            checkGuessStatus 
                answer 
                guessedWord 
                (fun g -> g.Letter1) // Right guess
                (fun g -> g.Letter2)
                (fun g -> g.Letter3)
                (fun g -> g.Letter4)
                (fun g -> g.Letter5)
        Letter2State = 
            checkGuessStatus 
                answer 
                guessedWord 
                (fun g -> g.Letter2) // Right guess
                (fun g -> g.Letter1)
                (fun g -> g.Letter3)
                (fun g -> g.Letter4)
                (fun g -> g.Letter5)
        Letter3State = 
            checkGuessStatus 
                answer 
                guessedWord 
                (fun g -> g.Letter3) // Right guess
                (fun g -> g.Letter2)
                (fun g -> g.Letter1)
                (fun g -> g.Letter4)
                (fun g -> g.Letter5)
        Letter4State = 
            checkGuessStatus 
                answer 
                guessedWord 
                (fun g -> g.Letter4) // Right guess
                (fun g -> g.Letter2)
                (fun g -> g.Letter3)
                (fun g -> g.Letter1)
                (fun g -> g.Letter5)
        Letter5State = 
            checkGuessStatus 
                answer 
                guessedWord 
                (fun g -> g.Letter5) // Right guess
                (fun g -> g.Letter2)
                (fun g -> g.Letter3)
                (fun g -> g.Letter4)
                (fun g -> g.Letter1)
    }

let tryAllWordsSimulation() = 
    let answer = Word.create "YEARN"
    let game = Wordle.createNewGame answer

    Array.fold (fun (wordle: Wordle) wordToGuess -> 
        if wordle.IsOver 
            then wordle
            else 
                let guessedWord = guess answer wordle.Guesses.Length wordToGuess
                { wordle with 
                    Guesses = Array.append wordle.Guesses [| guessedWord |]
                    IsOver = GuessedWord.isCorrect guessedWord
                }

        )
        game
        Words.validWords

//let answer = Word.create "YEARN"
//let guessTest = guess answer 1 "YEARN"

//printfn "%A" guessTest

let playedGame = tryAllWordsSimulation()

printfn "%A" playedGame.IsOver
printfn "%A" playedGame.Guesses.Length
printfn "%A" (playedGame.Guesses |> Array.find (fun g -> GuessedWord.isCorrect g))