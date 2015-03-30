module main

type note = 
    | C
    | Cis
    | D
    | Dis
    | E
    | F
    | Fis
    | G
    | Gis
    | A
    | Ais
    | B

type value =
    | Full
    | Half
    | Quarter

let frequency = 
    Map.ofList [ (C, 261.63)
                 (Cis, 277.18)
                 (D, 293.66)
                 (Dis, 311.13)
                 (E, 329.63)
                 (F, 349.23)
                 (Fis, 369.99)
                 (G, 392.00)
                 (Gis, 415.30)
                 (A, 440.00)
                 (Ais, 466.16)
                 (B, 493.88) ]

let time tempo =
    let quarterLength = 60. / tempo     
    Map.ofList [(Quarter, quarterLength); (Half, 2. * quarterLength); (Full, 4. * quarterLength)]

let nooaMelody = 
    [ (C, Quarter); (C, Quarter); (C, Quarter); (E, Quarter); 
    (D, Quarter); (D, Quarter); (D, Quarter); (F, Quarter); 
    (E, Quarter); (E, Quarter); (D, Quarter); (D, Quarter); 
    (C, Full); 
    (E, Quarter); (E, Quarter); (E, Quarter); (E, Quarter);
    (G, Half); (F, Half); 
    (D, Quarter); (D, Quarter); (D, Quarter); (D, Quarter); 
    (F, Half); (E, Half); 
    (C, Quarter); (C, Quarter); (C, Quarter); (E, Quarter);
    (D, Quarter); (D, Quarter); (D, Quarter); (F, Quarter);
    (E, Quarter); (E, Quarter); (D, Quarter); (D, Quarter); 
    (C, Full); ]

let createMarkovChains data =
    data
    |> Seq.windowed 2
    |> Seq.groupBy (fun x -> x.[0])
    |> Seq.map (fun x -> 
           (fst x, 
            x
            |> snd
            |> Seq.map (Seq.nth 1)))
    |> Map.ofSeq

let noteData = 
    nooaMelody 
    |> List.map(fst)
    |> createMarkovChains


let timingsData =
    nooaMelody
    |> List.map(snd)
    |> createMarkovChains

let getNextElement (random : System.Random) (data : Map<_,_>) currentElement = 
    let nextSet = data.[currentElement]
    let nextElementIndex = random.Next(0, Seq.length nextSet)
    nextSet
    |> Seq.skip nextElementIndex
    |> Seq.head

let r = System.Random()
let nextElementFromData x y = getNextElement r x y

let rec randomMelody wantedLength seedData currentElement (acc : 'a list) = 
    if acc.Length = wantedLength then acc
    else 
        let nextElement = nextElementFromData seedData currentElement
        randomMelody wantedLength seedData nextElement (acc @ [ nextElement ])

let printSonicPiFormat melody = 
    for note in melody do
        printfn "play :%A, release: %A" (fst note) (snd note)
        printfn "sleep %A" (snd note)

[<EntryPoint>]
let main argv = 
    let availableNotes = 
        noteData |> Map.toSeq |> Seq.map fst |> List.ofSeq
    let firstNote = availableNotes.[r.Next(0, availableNotes.Length)]
    let melody = randomMelody 8 noteData firstNote [ firstNote ]
    let availableTimes = 
        timingsData |> Map.toSeq |> Seq.map fst |> List.ofSeq
    let firstTime = availableTimes.[r.Next(0, availableTimes.Length)]
    let timings = randomMelody 8 timingsData firstTime [ firstTime ] |> List.map(fun x -> (time 140.).[x])
    List.zip melody timings |> printSonicPiFormat 
    List.zip (List.map(fun x -> frequency.[x]) melody) timings |> Synth.writeMelody
    0 // return an integer exit code
