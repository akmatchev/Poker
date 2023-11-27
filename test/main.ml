open Poker
open Cards
open Hands
open Random
open OUnit2

let royal_flush =
  [
    { rank = 10; suit = Spades };
    { rank = 11; suit = Spades };
    { rank = 12; suit = Spades };
    { rank = 13; suit = Spades };
    { rank = 14; suit = Spades };
  ]

let () = print_cards royal_flush
let starter_deck = shuffle_deck full_deck
let () = print_cards starter_deck
let flop, post_flop_deck = draw_flop starter_deck
let () = print_cards flop
let turn_card, post_turn_deck = draw_turn_river post_flop_deck
let turn = flop @ [ turn_card ]
let () = print_cards turn
let river_card, post_river_deck = draw_turn_river post_turn_deck
let river = turn @ [ river_card ]
let () = print_cards river

(**[pairTester] given some pairing function f and some list of cards will print
   out the rank of the pair of in cards if it exists, otherwise prints 0*)
let pairTester f cards =
  print_endline (string_of_int (Option.value (f cards) ~default:0))

(**[pairPairTester] given some double pairing function f and some list of cards
   will print out the rank of the pair of in cards if it exists, otherwise
   prints 0*)
let pairPairTester f cards =
  let part = Option.value (f cards) ~default:(0, 0) in
  print_endline (string_of_int (fst part) ^ "," ^ string_of_int (snd part))

let simple_one_pair =
  [
    { rank = 1; suit = Clubs };
    { rank = 2; suit = Diamonds };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 1; suit = Diamonds };
  ]

let () = pairTester one_pair simple_one_pair

let simple_two_pair =
  [
    { rank = 3; suit = Clubs };
    { rank = 2; suit = Diamonds };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 6; suit = Diamonds };
  ]

let () = pairPairTester two_pair simple_two_pair

let simple_three_of_a_kind =
  [
    { rank = 3; suit = Clubs };
    { rank = 6; suit = Spades };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 6; suit = Diamonds };
  ]

let () = pairTester three_of_kind simple_three_of_a_kind

let simple_four_of_a_kind =
  [
    { rank = 8; suit = Clubs };
    { rank = 8; suit = Spades };
    { rank = 3; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 8; suit = Hearts };
    { rank = 8; suit = Diamonds };
  ]

let () = pairTester four_of_kind simple_four_of_a_kind

let simple_full_house =
  [
    { rank = 7; suit = Clubs };
    { rank = 8; suit = Spades };
    { rank = 7; suit = Diamonds };
    { rank = 11; suit = Spades };
    { rank = 5; suit = Clubs };
    { rank = 8; suit = Hearts };
    { rank = 8; suit = Diamonds };
  ]

let () = pairPairTester full_house simple_full_house

(**[makeTestPair] returns list of two cards which are a one_pair*)
let makeTestPair () =
  let randRank = Random.int 13 in
  let randSuitNum = Random.int 4 in
  let randSuit = List.nth suits randSuitNum in
  let firs = { rank = randRank; suit = randSuit } in
  let randSecSuit = Random.int 3 in
  let sec =
    {
      rank = randRank;
      suit = List.nth (List.filter (fun x -> x <> randSuit) suits) randSecSuit;
    }
  in
  [ firs; sec ]

(**[makeRankCard] returns a card from a given rank and a random suit*)
let makeRankCard rank = { rank; suit = List.nth suits (Random.int 4) }

(**[exepRand] returns random number within 1 and bound including 1 and excluding
   bound, excluding any numbers in list exep*)
let rec exepRand exep bound =
  let num = Random.int bound + 1 in
  if List.mem num exep then exepRand exep bound else num

(**[randomNums] returns disinct list of numbers len long from 1 to bound
   excluding exep and bound*)
let rec randomNums len bound exep =
  if len > 0 then
    let num = exepRand exep bound in
    num :: randomNums (len - 1) bound (num :: exep)
  else []

(**[makeTestPairHand] returns random 7 card hand with a one_pair and no other
   pairings*)
let makeTestPairHand () =
  let pair = makeTestPair () in
  let nums = randomNums 5 13 [ (List.hd pair).rank ] in
  pair @ List.map (fun x -> makeRankCard x) nums

(**[print_some_cards] compactly prints cards*)
let print_some_cards cards =
  let helper acc card =
    acc ^ "(" ^ string_of_int card.rank ^ "," ^ suit_to_string card.suit ^ ")"
  in
  List.fold_left helper "" cards

let category_tests =
  [
    ( "the elements in the to_list are in order from the least key to the \
       greatest"
    >:: fun _ ->
      assert_equal
        ~printer:(fun x -> x)
        "One_Pair"
        (Cards.category_to_string (hand_category (makeTestPairHand ()))) );
  ]

let suite = "test suite" >::: List.flatten [ category_tests ]
let _ = run_test_tt_main suite

(* randomized test, goal would be to run this for large numbers *)
let () = pairTester one_pair (makeTestPairHand ())
