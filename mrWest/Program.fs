module main

type note = C | Cis | D | Dis | E | F | Fis | G | Gis | A | Ais | B

let frequency = Map.ofList [(C, 261.63); (Cis, 277.18); (D, 293.66); (Dis, 311.13); (E, 329.63);
                            (F, 349.23); (Fis, 369.99); (G, 392.00); (Gis, 415.30); (A, 440.00);
                            (Ais, 466.16); (B, 493.88)]

let cMajor = [C; D; E; F; G; A; B]

let randomMelody () = 
    let r = System.Random()
    let length = cMajor.Length
    seq {0 .. 7} |> Seq.map (fun x -> cMajor.[r.Next(0, length)])

[<EntryPoint>]
let main argv = 
    randomMelody() 
        |> Seq.map (fun x -> frequency.[x])
        |> Synth.writeMelody
    0 // return an integer exit code

