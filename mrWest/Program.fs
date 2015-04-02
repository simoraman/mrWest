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
    | Eighth

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

let time beatsPerMinute =
    let quarterLength = 60. / beatsPerMinute     
    Map.ofList [(Eighth, 0.5 * quarterLength); (Quarter, quarterLength); (Half, 2. * quarterLength); (Full, 4. * quarterLength)]

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

let yesterday = 
    [(G, Eighth); (F, Eighth); (F, Half);
    (A, Eighth); (B, Eighth);(Cis, Eighth); (D, Eighth); (E, Eighth);(F, Eighth);
    (E, Eighth); (D, Eighth); (D, Half);
    (D, Eighth); (D, Eighth); (C, Eighth); (Ais, Eighth); (A, Eighth); (G, Eighth);
    (Ais, Quarter); (A, Eighth); (A, Quarter); (G, Quarter);
    (F, Quarter); (A, Eighth); (G, Eighth); (G, Quarter); (D, Eighth);
    (F, Quarter); (A, Eighth); (A, Eighth); (A, Half);]

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
    yesterday
    |> List.map(fst)
    |> createMarkovChains

let timingsData =
    yesterday
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

let rec randomSequence wantedLength seedData currentElement (acc : 'a list) = 
    if acc.Length = wantedLength then acc
    else 
        let nextElement = nextElementFromData seedData currentElement
        randomSequence wantedLength seedData nextElement (acc @ [ nextElement ])

[<EntryPoint>]
let main argv = 
    let availableNotes = 
        noteData |> Map.toSeq |> Seq.map fst |> List.ofSeq
    let firstNote = availableNotes.[r.Next(0, availableNotes.Length)]
    let melody = randomSequence 24 noteData firstNote [ firstNote ] 
                    |> List.map(fun x -> frequency.[x])

    let timingsForTempo = (time 120.)
    let availableTimes = 
        timingsData |> Map.toSeq |> Seq.map fst |> List.ofSeq
    let firstTime = availableTimes.[r.Next(0, availableTimes.Length)]
    let timings = randomSequence 24 timingsData firstTime [ firstTime ] 
                    |> List.map(fun x -> timingsForTempo.[x])

    List.zip melody timings |> Synth.writeMelody
    0
