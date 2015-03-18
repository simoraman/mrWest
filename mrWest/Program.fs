module main

type note = C | Cis | D | Dis | E | F | Fis | G | Gis | A | Ais | B

let frequency = Map.ofList [(C, 261.63); (Cis, 277.18); (D, 293.66); (Dis, 311.13); (E, 329.63);
                            (F, 349.23); (Fis, 369.99); (G, 392.00); (Gis, 415.30); (A, 440.00);
                            (Ais, 466.16); (B, 493.88)]

let cMajor = [C; D; E; F; G; A; B]

let nooaMelody = [C; C; C; E; D; D; D; F; E; E; D; D; C;
E; E; E; E; G; F; D; D; D; D; F; E;
C; C; C; E; D; D; D; F; E; E; D; D; C;]

let grouped = nooaMelody |> Seq.windowed 2 |> Seq.groupBy (fun (x:note[]) -> x.[0]) 

let noteData = grouped |> Seq.map (fun x -> (fst x, x |> snd |> Seq.map (Seq.nth 1))) 

let getNextNote (random:System.Random) (data:seq<note * seq<note>>) (currentNote:note) =
    let nextSet:seq<note> = data |> Seq.find (fun x -> (fst x) = currentNote) |> snd
    let nextNoteIndex = random.Next(0, Seq.length nextSet)
    nextSet |> Seq.skip nextNoteIndex |> Seq.head

let r = System.Random()
let nextNoteApplied : (seq<note * seq<note>> -> note -> note) = getNextNote r
let rec randomMelo data current (melo:note list) = 
    match melo.Length with
    | 8 -> melo
    | _ -> 
        let nextNote = nextNoteApplied data current
        randomMelo data nextNote (melo @ [nextNote])

let improvedMelody =
    let r = System.Random()
    let length = cMajor.Length
    let firstNote = cMajor.[r.Next(0, length)]
    randomMelo noteData firstNote [firstNote]

[<EntryPoint>]
let main argv = 
    improvedMelody 
        |> Seq.map (fun x -> frequency.[x])
        |> Synth.writeMelody
    0 // return an integer exit code
