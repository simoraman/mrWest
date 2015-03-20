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

let nooaMelody = 
    [ C; C; C; E; D; D; D; F; E; E; D; D; C; 
    E; E; E; E; G; F; D; D; D; D; F; E; 
    C; C; C; E; D; D; D; F; E; E; D; D; C ]

let noteData = 
    nooaMelody
    |> Seq.windowed 2
    |> Seq.groupBy (fun (x : note []) -> x.[0])
    |> Seq.map (fun x -> 
           (fst x, 
            x
            |> snd
            |> Seq.map (Seq.nth 1)))
    |> Map.ofSeq

let getNextNote (random : System.Random) (data : Map<note,seq<note>>) (currentNote : note) = 
    let nextSet : seq<note> = data.[currentNote]
    let nextNoteIndex = random.Next(0, Seq.length nextSet)
    nextSet
    |> Seq.skip nextNoteIndex
    |> Seq.head

let r = System.Random()
let nextNoteFromData : Map<note,seq<note>> -> note -> note = getNextNote r

let rec randomMelody wantedLength noteData currentNote (melody : note list) = 
    if melody.Length = wantedLength then melody
    else 
        let nextNote = nextNoteFromData noteData currentNote
        randomMelody wantedLength noteData nextNote (melody @ [ nextNote ])

[<EntryPoint>]
let main argv = 
    let availableNotes = noteData |> Map.toSeq |> Seq.map fst |> List.ofSeq
    let firstNote = availableNotes.[r.Next(0, availableNotes.Length)]
    randomMelody 8 noteData firstNote [ firstNote ]
    |> Seq.map (fun x -> frequency.[x])
    |> Synth.writeMelody
    0 // return an integer exit code
