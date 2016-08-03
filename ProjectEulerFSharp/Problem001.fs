module Problem001
(*
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
*)
let isMultiple x = (x % 3 = 0) || (x % 5 = 0)
let multipleSum n = [1..n] 
                    |> Seq.filter isMultiple
                    |> Seq.sum
let solution = multipleSum (1000-1)