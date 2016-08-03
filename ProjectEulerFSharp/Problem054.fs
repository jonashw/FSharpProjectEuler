module Problem054
(*
In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins;
for example, a pair of eights beats a pair of fives (see example 1 below).
But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
if the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

Hand	Player 1	 	     Player 2	 	     Winner
---------------------------------------------------------
1	 	5H 5C 6S 7S KD 	     2C 3S 8S 8D TD      Player 2
        Pair of Fives        Pair of Eights 

2	 	5D 8C 9S JS AC       2C 5C 7D 8S QH      Player 1
        Highest card Ace     Highest card Queen

3	 	2D 9C AS AH AC       3D 6D 7D TD QD      Player 2
        Three Aces           Flush with Diamonds

4	 	4D 6S 9H QH QC       3D 6D 7H QD QS      Player 1
        Pair of Queens       Pair of Queens
        Highest card Nine    Highest card Seven

5	 	2H 2D 4C 4D 4S       3C 3D 3S 9S 9D      Player 1
        Full House           Full House
        With Three Fours     with Three Threes
 	
The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space):
    the first five are Player 1's cards and the last five are Player 2's cards.

You can assume that all hands are valid (no invalid characters or repeated cards),
each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?
*)

type Suit = Clubs | Diamonds | Hearts | Spades
            override this.ToString() =
                match this with
                | Clubs    -> "C"
                | Diamonds -> "D"
                | Hearts   -> "H"
                | Spades   -> "S"
            static member parse s =
                match s with
                | 'C' -> Clubs
                | 'D' -> Diamonds
                | 'H' -> Hearts
                | 'S' -> Spades
                | _   -> raise (System.ArgumentException("Unexpected Suit code: " + s.ToString()))

type Value =
    | Two | Three | Four 
    | Five | Six | Seven 
    | Eight | Nine | Ten 
    | Jack | Queen | King 
    | Ace
    member this.toInt =
        match this with
        | Two   -> 2  | Three -> 3  | Four  -> 4
        | Five  -> 5  | Six   -> 6  | Seven -> 7
        | Eight -> 8  | Nine  -> 9  | Ten   -> 10
        | Jack  -> 11 | Queen -> 12 | King  -> 13
        | Ace   -> 14
    override this.ToString() =
        match this with
        | Two   -> "2" | Three -> "3" | Four  -> "4"
        | Five  -> "5" | Six   -> "6" | Seven -> "7"
        | Eight -> "8" | Nine  -> "9" | Ten   -> "T"
        | Jack  -> "J" | Queen -> "Q" | King  -> "K"
        | Ace   -> "A"
    static member parse v =
        match v with
        | '2' -> Two   | '3' -> Three | '4' -> Four
        | '5' -> Five  | '6' -> Six   | '7' -> Seven
        | '8' -> Eight | '9' -> Nine  | 'T' -> Ten
        | 'J' -> Jack  | 'Q' -> Queen | 'K' -> King
        | 'A' -> Ace
        | _ -> raise (System.ArgumentException("Unrecognized card value code: " + v.ToString()))
    static member Ordered = 
        [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]

type Card = Value * Suit

let parseCard (str: string): Card =
    match (str.ToCharArray() |> List.ofArray) with
    | [v; s] -> (Value.parse v, Suit.parse s)
    | _ -> raise (System.ArgumentException("Invalid card code. A valid codes is 2 characters in length. This code length = " + str.Length.ToString()))

type Hand =
    | HighCard of Value
    | OnePair of pairValue: Value * withHigh: Value
    | TwoPair of Value * Value
    | ThreeOfAKind of Value
    | Straight of high: Value
    | Flush of Suit
    | FullHouse of threeOf: Value * twoOf: Value
    | FourOfAKind of Value
    | StraightFlush of high: Value * suit: Suit
    | RoyalFlush of Suit 
    with member this.Name =
          match this with
          | HighCard(_)        -> "High Card"
          | OnePair(_,_)       -> "Pair"
          | TwoPair(_,_)       -> "2 Pairs"
          | ThreeOfAKind(_)    -> "Three of a Kind"
          | Straight(_)        -> "Straight"
          | Flush(_)           -> "Flush"
          | FullHouse(_,_)     -> "Full House"
          | FourOfAKind(_)     -> "Four of a Kind"
          | StraightFlush(_,_) -> "Straight Flush"
          | RoyalFlush(_)      -> "Royal Flush"

let tryGetFlush (ss: Suit list): Suit option =
    ss |> Seq.distinct 
       |> List.ofSeq
       |> function 
          | [suit] -> Some(suit) 
          | _ -> None

let tryGetStraightHigh (vs: Value list): Value option =
    let sorted = List.sort vs
    let straightStart = List.head sorted 
    let wouldBeStraight = Value.Ordered 
                          |> List.skipWhile (fun v -> v <> straightStart)
                          |> List.truncate 5
    if sorted = wouldBeStraight
    then Some(List.last sorted)
    else None

let handFromCards (cs: Card list): Hand = 
      let orderedValues = cs |> Seq.map fst |> List.ofSeq |> List.sortDescending 
      let flush  = cs |> List.map snd |> tryGetFlush
      let straight = tryGetStraightHigh orderedValues 
      match flush, straight with
      | Some(suit), Some(Ace)  -> RoyalFlush(suit)
      | Some(suit), Some(high) -> StraightFlush(suit = suit, high = high)
      | Some(suit), None       -> Flush(suit)
      | None,       Some(high) -> Straight(high = high)
      | None,       None       -> orderedValues 
                                  |> List.groupBy id 
                                  |> List.map (fun (value, vs) -> (value, List.length vs)) 
                                  |> List.sortByDescending snd
                                  |> function
                                     | [(fourOf, 4); _]                -> FourOfAKind(fourOf)
                                     | [(threeOf, 3); (twoOf, 2)]      -> FullHouse(threeOf, twoOf)
                                     | (threeOf, 3) :: _               -> ThreeOfAKind(threeOf)
                                     | (twoOfA, 2) :: (twoOfB, 2) :: _ -> TwoPair(twoOfA, twoOfB)
                                     | (twoOf, 2) :: _                 -> OnePair(twoOf, orderedValues |> List.skip 2 |> List.head)
                                     | _                               -> HighCard(orderedValues |> List.head)

type Player = Player1 | Player2
              with override this.ToString() = 
                      match this with 
                      | Player1 -> "Player 1"
                      | Player2 -> "Player 2"

type Game(player1: Card list, player2: Card list) =
    member this.Player1Cards = player1
    member this.Player2Cards = player2
    member this.Player1Hand = player1 |> handFromCards
    member this.Player2Hand = player2 |> handFromCards
    member this.Winner = if this.Player1Hand > this.Player2Hand
                         then Player1
                         else Player2
    member this.WinnerWithHand = if this.Player1Hand > this.Player2Hand 
                                 then (Player1, this.Player1Hand)
                                 else (Player2, this.Player2Hand)
    static member parse (str: string) =
        let looseCards = str.Split(' ') |> Seq.map parseCard |> Seq.toArray
        let player1Hand = Seq.take 5 looseCards |> List.ofSeq
        let player2Hand = Seq.skip 5 looseCards |> List.ofSeq
        Game(player1Hand, player2Hand)

//"Tests" from the problem description:
let pairs =            Game.parse "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
let aceVsQueen =       Game.parse "5D 8C 9S JS AC 2C 5C 7D 8S QH"
let flushVsThreeAces = Game.parse "2D 9C AS AH AC 3D 6D 7D TD QD"
let pairsWithHighest = Game.parse "4D 6S 9H QH QC 3D 6D 7H QD QS"
let fullHouses =       Game.parse "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
let games = [pairs; aceVsQueen; flushVsThreeAces; pairsWithHighest; fullHouses]
let winners = games |> List.map (fun g -> g.Winner)
//Other "tests"
let royalFlushes =     Game.parse "TH JH QH KH AH TC JC QC KC AC"
let straightFlushes =  Game.parse "9H TH JH QH KH 9C TC JC QC KC"
let flushes =          Game.parse "2H 5H 8H 9H AH 5C 6C 8C TC KC"
let straights =        Game.parse "2H 3C 4D 5S 6H 9C TD JC QH KS"
let tie =              Game.parse "2C 2C 2C 2C 2C 2C 2C 2C 2C 2C"
let fours =            Game.parse "5H 5C 5S 5D 2C 9H 9C 9S 9D 2H"
let threes =           Game.parse "JH JC JS 4D 2C KH KC KS 3D 2H"

let solutionGames = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Problem054-games.txt")
    |> System.IO.File.ReadLines 
    |> List.ofSeq
    |> List.map Game.parse

let handRank h =
      match h with
      | HighCard(_)          -> 0
      | OnePair(_,_)         -> 1
      | TwoPair(_,_)         -> 2
      | ThreeOfAKind(_)      -> 3
      | Straight(_)          -> 4
      | Flush(_)             -> 5
      | FullHouse(_,_)       -> 6
      | FourOfAKind(_)       -> 7
      | StraightFlush(_,_)   -> 8
      | RoyalFlush(_)        -> 9

let statistics =
    solutionGames
    |> List.collect (fun g -> [(Player1, g.Player1Hand); (Player2, g.Player2Hand)])
    |> List.groupBy fst
    |> List.map (fun (player, hands) -> 
        let gs = hands |> List.map snd
                       |> List.groupBy (fun hand -> (handRank hand, hand.Name))
                       |> List.map (fun ((handRank, handName), hands) -> ((handRank, handName), Seq.length hands))
                       |> List.sortByDescending (fun ((handRank, _), _) -> handRank)
                       |> List.map (fun ((_, handName), handCount) -> (handName, handCount))
        (player, gs))

let printStats =
    statistics |> List.iter 
        (fun (player, handCategories) -> 
            System.Console.WriteLine("==========")
            System.Console.WriteLine(player.ToString())
            System.Console.WriteLine("----------")
            handCategories |> List.iter (fun (handName, count) ->
                System.Console.WriteLine(handName + ": " + count.ToString())
            )
            System.Console.WriteLine())

let solution = 
    solutionGames
    |> List.filter (fun g -> g.Winner = Player1)
    |> List.length