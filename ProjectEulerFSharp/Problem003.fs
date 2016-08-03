module Problem003

(*  LARGEST PRIME FACTOR 
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143?
*)
let sievePrimesUpTo n = 
    let sieve = new System.Collections.Generic.Dictionary<int64,bool>()
    [1L..n] |> List.iter (fun x -> sieve.[x] <- true)
    let pMax = n |> (float) |> sqrt |> (int64)
    let ps = [2L..pMax]
    ps |> Seq.filter (fun p -> sieve.[p])
       |> Seq.collect (fun p -> [p*p..p..n])
       |> Seq.iter (fun x -> sieve.[x] <- false)
    sieve |> Seq.filter (fun pair -> pair.Value)
          |> Seq.map (fun pair -> pair.Key)
          |> List.ofSeq

let isFactor n f = (((float) n) / ((float) f)) |> (fun x -> x - floor x) = 0.0

let primeFactors n = sievePrimesUpTo n |> List.filter (isFactor n)
    
let solution = primeFactors 600851475143L
/// Problem: OutOfMemoryException