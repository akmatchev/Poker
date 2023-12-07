(**TESTING PLAN: We manually tested the functions in: cards.ml, game.ml,
   player.ml, and wrote OUnit tests for: hands.ml. We decided to manually test
   cards.ml because it was very straightforward to manually test and we had made
   a plan beforehand that the majority of our OUnit tests would be for hand
   evaluation in hands.ml, since there are many different cases of comparing
   hands we could test to achieve the minimum 50 passing test cases. game.ml is
   virtually impossible to test through OUnit because we are shuffling the deck
   before the start of the round, so there is no expected output. We manually
   tested player.ml because there are so few functions that we ended up using.
   We initially had more functions in player.ml that we had tests for, but they
   served no purpose in our final deliverable so we ended up deleting those
   tests. For OUnit tests, we used glass-box testing for the helper functions
   like [category_to_string] to get a sense of how they were implemented/how
   they operated, and once we knew that those worked, we used a combination of
   black-box and glass-box testing for [is_category_tests],
   [hand_category_tests], and [best_hand_tests]. We initially had some
   randomized tests when first testing the category of different hands but we
   ended up not using them after we changed our hand implementation. Our testing
   approach shows correctness of our system because we used a combination of
   black-box and glass-box testing, which is the recommended approach for
   achieving the best functionality. To begin with, we began testing our helper
   functions to ensure that those performed their desired functinality, so that
   we could correctly use them for our black-box/glass-box tests for the
   [hand_category] and [best_hand_tests] For our black-box tests, we wrote them
   before actually implementing the functions we were testing, which
   demonstrates that our functions perform well regardless of how they were
   implemented. For our glass-box tests, we were able to take into account how
   our functions were implemented and write our tests in a manner that
   complemented our implementation. In both cases, we wrote down the different
   cases in which hands were being compared and included cases where hands were
   worse than, better than, or equal to our expected output in our test cases.
   This ensured that our functions were performing as intended and demonstrates
   the correctness of our system. *)

open Poker
open Cards
open Hands
open Player
open Random
open OUnit2

let card1 = { rank = 11; suit = Spades }
let card2 = { rank = 12; suit = Clubs }
let card3 = { rank = 11; suit = Diamonds }
let card4 = { rank = 10; suit = Spades }
let card5 = { rank = 7; suit = Diamonds }

let high_hand =
  [
    { rank = 9; suit = Spades };
    { rank = 13; suit = Hearts };
    { rank = 10; suit = Clubs };
    { rank = 2; suit = Hearts };
    { rank = 3; suit = Clubs };
  ]

let pair = [ card1; card2; card3; card4; card5 ]

let two_pair =
  [
    { rank = 5; suit = Spades };
    { rank = 4; suit = Hearts };
    { rank = 5; suit = Spades };
    { rank = 4; suit = Clubs };
    { rank = 3; suit = Hearts };
  ]

let three_of_a_kind =
  [
    { rank = 4; suit = Spades };
    { rank = 4; suit = Hearts };
    { rank = 13; suit = Spades };
    { rank = 4; suit = Clubs };
    { rank = 3; suit = Hearts };
  ]

let straight =
  [
    { rank = 7; suit = Diamonds };
    { rank = 9; suit = Clubs };
    { rank = 6; suit = Spades };
    { rank = 8; suit = Clubs };
    { rank = 5; suit = Clubs };
  ]

let special_straight =
  [
    { rank = 3; suit = Spades };
    { rank = 14; suit = Hearts };
    { rank = 5; suit = Spades };
    { rank = 2; suit = Clubs };
    { rank = 4; suit = Clubs };
  ]

let diamond_flush =
  [
    card3;
    card5;
    { rank = 13; suit = Diamonds };
    { rank = 5; suit = Diamonds };
    { rank = 2; suit = Diamonds };
  ]

let full_house =
  [
    { rank = 7; suit = Spades };
    { rank = 7; suit = Hearts };
    { rank = 2; suit = Spades };
    { rank = 2; suit = Clubs };
    { rank = 7; suit = Diamonds };
  ]

let four_of_a_kind =
  [
    { rank = 4; suit = Spades };
    { rank = 4; suit = Hearts };
    { rank = 3; suit = Spades };
    { rank = 4; suit = Clubs };
    { rank = 4; suit = Diamonds };
  ]

let straight_flush =
  [
    { rank = 13; suit = Hearts };
    { rank = 9; suit = Hearts };
    { rank = 11; suit = Hearts };
    { rank = 12; suit = Hearts };
    { rank = 10; suit = Hearts };
  ]

let special_straight_flush =
  [
    { rank = 3; suit = Clubs };
    { rank = 14; suit = Clubs };
    { rank = 5; suit = Clubs };
    { rank = 2; suit = Clubs };
    { rank = 4; suit = Clubs };
  ]

let royal_flush =
  [
    { rank = 13; suit = Spades };
    { rank = 11; suit = Spades };
    { rank = 10; suit = Spades };
    { rank = 14; suit = Spades };
    { rank = 12; suit = Spades };
  ]

let helper_tests =
  [
    ( "category_to_string test" >:: fun _ ->
      assert_equal "High_Card"
        (category_to_string
           (High { hcard1 = 10; hcard2 = 9; hcard3 = 8; hcard4 = 3; hcard5 = 2 }))
    );
    ( "compare_by_rank less than test" >:: fun _ ->
      assert_equal (-1) (compare_by_rank card1 card2) );
    ( "compare_by_rank equal to test" >:: fun _ ->
      assert_equal 0 (compare_by_rank card1 card3) );
    ( "compare by rank greater than test" >:: fun _ ->
      assert_equal 1 (compare_by_rank card2 card3) );
    ( "sort_by_rank size 3: simple test" >:: fun _ ->
      assert_equal [ card2; card3; card5 ]
        (sort_by_rank [ card5; card2; card3 ]) );
    ( "sort by rank size 3: equal rank test " >:: fun _ ->
      assert_equal true
        (let lst = sort_by_rank [ card5; card3; card1 ] in
         lst = [ card1; card3; card5 ] || lst = [ card3; card1; card5 ]) );
    ( "sort by rank size 5 test" >:: fun _ ->
      assert_equal true
        (let lst = sort_by_rank [ card5; card4; card3; card2; card1 ] in
         lst = [ card2; card1; card3; card4; card5 ]
         || lst = [ card2; card3; card1; card4; card5 ]) );
    ( "cards to ranks test" >:: fun _ ->
      assert_equal [ 11; 12; 11; 10; 7 ] (cards_to_ranks pair) );
  ]

let card_combinations_test =
  [
    ( "card_combo test" >:: fun _ ->
      assert_equal 21
        (let player_hand = [ card1; card2 ] in
         let board =
           [
             { rank = 2; suit = Spades };
             { rank = 5; suit = Diamonds };
             { rank = 10; suit = Diamonds };
             { rank = 11; suit = Spades };
             { rank = 5; suit = Clubs };
           ]
         in
         List.length (card_combinations board player_hand)) );
  ]

let category_helper_tests =
  [
    ( "flush_checker test 1: empty card list" >:: fun _ ->
      assert_equal true (flush_checker []) );
    ( "flush_checker test 2: one card" >:: fun _ ->
      assert_equal true (flush_checker [ card1 ]) );
    ( "flush_checker test 3: normal five card hand" >:: fun _ ->
      assert_equal false (flush_checker pair) );
    ( "flush_checker test 4: diamond flush" >:: fun _ ->
      assert_equal true (flush_checker diamond_flush) );
    ( "flush_checker test 5: straight flush" >:: fun _ ->
      assert_equal true (flush_checker straight_flush) );
    ( "straight_checker test 1: one card" >:: fun _ ->
      assert_equal 11 (straight_checker [ card1 ]) );
    ( "straight_checker test 2: normal 5 card hand" >:: fun _ ->
      assert_equal 0 (straight_checker pair) );
    ( "straight_checker test 3: regular straight" >:: fun _ ->
      assert_equal 9 (straight_checker straight) );
    ( "straight_checker test 4: straight flush" >:: fun _ ->
      assert_equal 13 (straight_checker straight_flush) );
    ( "straight_checker test 5: special straight" >:: fun _ ->
      assert_equal (-100) (straight_checker special_straight) );
  ]

let is_category_tests =
  [
    ( "is_royal_flush test 1: normal hand" >:: fun _ ->
      assert_equal None (is_royal_flush pair) );
    ( "is_royal_flush test 2: royal flush" >:: fun _ ->
      assert_equal (Some Royal_Flush) (is_royal_flush royal_flush) );
    ( "is_straight_flush test 1: normal hand" >:: fun _ ->
      assert_equal None (is_royal_flush pair) );
    ( "is_straight_flush test 2: flush but not straight" >:: fun _ ->
      assert_equal None (is_straight_flush diamond_flush) );
    ( "is_straight_flush test 3: straight but not flush" >:: fun _ ->
      assert_equal None (is_straight_flush straight) );
    ( "is_straight_flush test 4: straight flush" >:: fun _ ->
      assert_equal
        (Some (Straight_Flush { hcard = 13 }))
        (is_straight_flush straight_flush) );
    ( "is_straight_flush test 5: special straight flush" >:: fun _ ->
      assert_equal
        (Some (Straight_Flush { hcard = -100 }))
        (is_straight_flush special_straight_flush) );
    ( "is_four_kind test 1: normal hand" >:: fun _ ->
      assert_equal None (is_four_of_kind pair) );
    ( "is_four_kind test 2: three of a kind" >:: fun _ ->
      assert_equal None (is_four_of_kind three_of_a_kind) );
    ( "is_four_kind test 3: four of a kind" >:: fun _ ->
      assert_equal
        (Some (Four_Of_A_Kind { four_kind = 4; hcard = 3 }))
        (is_four_of_kind four_of_a_kind) );
    ( "is_full_house test 1: normal hand" >:: fun _ ->
      assert_equal None (is_full_house pair) );
    ( "is_full_house test 2: three of a kind" >:: fun _ ->
      assert_equal None (is_full_house three_of_a_kind) );
    ( "is_full_house test 3: full house" >:: fun _ ->
      assert_equal
        (Some (Full_House { three_kind = 7; pair = 2 }))
        (is_full_house full_house) );
    ( "is_flush test 1: normal hand" >:: fun _ ->
      assert_equal None (is_flush pair) );
    ( "is_flush test 2: flush" >:: fun _ ->
      assert_equal
        (Some
           (Flush
              { hcard1 = 13; hcard2 = 11; hcard3 = 7; hcard4 = 5; hcard5 = 2 }))
        (is_flush diamond_flush) );
    ( "is_flush test 3: straight flush. Should be NO" >:: fun _ ->
      assert_equal None (is_flush straight_flush) );
    ( "is_straight test 1: normal hand" >:: fun _ ->
      assert_equal None (is_straight pair) );
    ( "is_straight test 2: straight" >:: fun _ ->
      assert_equal (Some (Straight { hcard = 9 })) (is_straight straight) );
    ( "is_straight test 3: straight flush. Should be NO" >:: fun _ ->
      assert_equal None (is_straight straight_flush) );
    ( "is_straight test 4: special straight" >:: fun _ ->
      assert_equal
        (Some (Straight { hcard = -100 }))
        (is_straight special_straight) );
    ( "is_three_of_kind test 1: normal hand" >:: fun _ ->
      assert_equal None (is_three_of_kind pair) );
    ( "is_three_of_kind test 2: two pair" >:: fun _ ->
      assert_equal None (is_three_of_kind two_pair) );
    ( "is_three_of_kind test 3: three of kind" >:: fun _ ->
      assert_equal
        (Some (Three_Of_A_Kind { three_kind = 4; hcard1 = 13; hcard2 = 3 }))
        (is_three_of_kind three_of_a_kind) );
    ( "is_three_of_kind test 4: four of kind. Should be NO" >:: fun _ ->
      assert_equal None (is_three_of_kind four_of_a_kind) );
    ( "is_three_of_kind test 5: full house. Should be NO" >:: fun _ ->
      assert_equal None (is_three_of_kind full_house) );
    ( "is_two_pair test 1: normal hand" >:: fun _ ->
      assert_equal None (is_two_pair pair) );
    ( "is_two_pair test 2: two pair" >:: fun _ ->
      assert_equal
        (Some (Two_Pair { pair1 = 5; pair2 = 4; hcard = 3 }))
        (is_two_pair two_pair) );
    ( "is_two_pair test 3: full house. Should be NO" >:: fun _ ->
      assert_equal None (is_two_pair full_house) );
    ( "is_pair test 1: high card" >:: fun _ ->
      assert_equal None (is_pair high_hand) );
    ( "is_pair test 2: one pair" >:: fun _ ->
      assert_equal
        (Some (One_Pair { pair = 11; hcard1 = 12; hcard2 = 10; hcard3 = 7 }))
        (is_pair pair) );
    ( "is_pair test 3: two pair. Should be NO" >:: fun _ ->
      assert_equal None (is_pair two_pair) );
    ( "is_pair test 4: three of a kind. Should be NO" >:: fun _ ->
      assert_equal None (is_pair three_of_a_kind) );
    ( "is_pair test 5: four of a kind. Should be NO" >:: fun _ ->
      assert_equal None (is_pair four_of_a_kind) );
    ( "is_pair test 6: full house. Should be NO" >:: fun _ ->
      assert_equal None (is_pair full_house) );
    ( "is_high_card test 1: high card" >:: fun _ ->
      assert_equal
        (Some
           (High
              { hcard1 = 13; hcard2 = 10; hcard3 = 9; hcard4 = 3; hcard5 = 2 }))
        (is_high_card high_hand) );
    ( "is_high_card test 2: pair hand. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card pair) );
    ( "is_high_card test 3: two pair. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card two_pair) );
    ( "is_high_card test 4: three of a kind. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card three_of_a_kind) );
    ( "is_high_card test 5: straight. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card straight) );
    ( "is_high_card test 6: flush. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card diamond_flush) );
    ( "is_high_card test 7: full house. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card full_house) );
    ( "is_high_card test 8: four of a kind. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card four_of_a_kind) );
    ( "is_high_card test 9: straight flush. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card two_pair) );
    ( "is_high_card test 10: royal flush. Should be NO" >:: fun _ ->
      assert_equal None (is_high_card two_pair) );
  ]

let hand_category_tests =
  [
    ( "high_card test" >:: fun _ ->
      assert_equal
        (High { hcard1 = 13; hcard2 = 10; hcard3 = 9; hcard4 = 3; hcard5 = 2 })
        (hand_category high_hand) );
    ( "pair test" >:: fun _ ->
      assert_equal
        (One_Pair { pair = 11; hcard1 = 12; hcard2 = 10; hcard3 = 7 })
        (hand_category pair) );
    ( "two pair test" >:: fun _ ->
      assert_equal
        (Two_Pair { pair1 = 5; pair2 = 4; hcard = 3 })
        (hand_category two_pair) );
    ( "three of a kind test" >:: fun _ ->
      assert_equal
        (Three_Of_A_Kind { three_kind = 4; hcard1 = 13; hcard2 = 3 })
        (hand_category three_of_a_kind) );
    ( "straight test" >:: fun _ ->
      assert_equal (Straight { hcard = 9 }) (hand_category straight) );
    ( "special straight test" >:: fun _ ->
      assert_equal (Straight { hcard = -100 }) (hand_category special_straight)
    );
    ( "flush test" >:: fun _ ->
      assert_equal
        (Flush { hcard1 = 13; hcard2 = 11; hcard3 = 7; hcard4 = 5; hcard5 = 2 })
        (hand_category diamond_flush) );
    ( "full house" >:: fun _ ->
      assert_equal
        (Full_House { three_kind = 7; pair = 2 })
        (hand_category full_house) );
    ( "four of a kind test" >:: fun _ ->
      assert_equal
        (Four_Of_A_Kind { four_kind = 4; hcard = 3 })
        (hand_category four_of_a_kind) );
    ( "straight flush test" >:: fun _ ->
      assert_equal
        (Straight_Flush { hcard = 13 })
        (hand_category straight_flush) );
    ( "special straight flush test" >:: fun _ ->
      assert_equal
        (Straight_Flush { hcard = -100 })
        (hand_category special_straight_flush) );
    ( "royal flush test" >:: fun _ ->
      assert_equal Royal_Flush (hand_category royal_flush) );
  ]

let board1 =
  [
    { rank = 12; suit = Spades };
    { rank = 14; suit = Clubs };
    { rank = 6; suit = Clubs };
    { rank = 2; suit = Clubs };
    { rank = 5; suit = Clubs };
  ]

let board2 = [ card1; card2; card3; card4 ]

let board3 =
  [
    { rank = 6; suit = Hearts };
    { rank = 2; suit = Hearts };
    { rank = 6; suit = Clubs };
    { rank = 2; suit = Clubs };
    { rank = 5; suit = Clubs };
  ]

let board4 =
  [
    { rank = 14; suit = Hearts };
    { rank = 10; suit = Clubs };
    { rank = 11; suit = Diamonds };
    { rank = 8; suit = Spades };
    { rank = 12; suit = Diamonds };
  ]

let board5 =
  [
    { rank = 11; suit = Spades };
    { rank = 11; suit = Diamonds };
    { rank = 4; suit = Clubs };
    { rank = 4; suit = Hearts };
    { rank = 7; suit = Spades };
  ]

let board6 =
  [
    { rank = 13; suit = Spades };
    { rank = 13; suit = Diamonds };
    { rank = 13; suit = Clubs };
    { rank = 4; suit = Hearts };
    { rank = 7; suit = Spades };
  ]

let board7 =
  [
    { rank = 2; suit = Spades };
    { rank = 7; suit = Diamonds };
    { rank = 8; suit = Clubs };
    { rank = 9; suit = Hearts };
    { rank = 10; suit = Spades };
  ]

let board8 =
  [
    { rank = 2; suit = Diamonds };
    { rank = 7; suit = Diamonds };
    { rank = 8; suit = Diamonds };
    { rank = 9; suit = Diamonds };
    { rank = 10; suit = Diamonds };
  ]

let player_hand1 =
  [ { rank = 10; suit = Hearts }; { rank = 4; suit = Diamonds } ]

let player_hand2 = [ { rank = 6; suit = Hearts }; { rank = 8; suit = Hearts } ]

let player_hand3 =
  [ { rank = 2; suit = Diamonds }; { rank = 5; suit = Diamonds } ]

let player_hand4 =
  [ { rank = 6; suit = Diamonds }; { rank = 6; suit = Spades } ]

let player_hand5 =
  [ { rank = 3; suit = Diamonds }; { rank = 4; suit = Diamonds } ]

let player_hand6 = [ { rank = 3; suit = Clubs }; { rank = 4; suit = Diamonds } ]

let player_hand7 =
  [ { rank = 11; suit = Clubs }; { rank = 12; suit = Diamonds } ]

let player_hand8 = [ { rank = 3; suit = Clubs }; { rank = 4; suit = Clubs } ]
let player_hand9 = [ { rank = 2; suit = Hearts }; { rank = 8; suit = Clubs } ]

let player_hand10 =
  [ { rank = 2; suit = Spades }; { rank = 14; suit = Spades } ]

let player_hand11 = [ { rank = 2; suit = Spades }; { rank = 3; suit = Spades } ]

let player_hand12 =
  [ { rank = 2; suit = Spades }; { rank = 2; suit = Diamonds } ]

let player_hand13 =
  [ { rank = 11; suit = Spades }; { rank = 8; suit = Diamonds } ]

let player_hand14 =
  [ { rank = 7; suit = Spades }; { rank = 11; suit = Diamonds } ]

let player_hand15 =
  [ { rank = 2; suit = Diamonds }; { rank = 11; suit = Diamonds } ]

let best_player_hand_tests =
  [
    ( "best_player_hand test 1: high card" >:: fun _ ->
      assert_equal
        (High { hcard1 = 14; hcard2 = 12; hcard3 = 10; hcard4 = 6; hcard5 = 5 })
        (best_player_hand board1 player_hand1) );
    ( "best_player_hand test 2: pair" >:: fun _ ->
      assert_equal
        (One_Pair { pair = 6; hcard1 = 14; hcard2 = 12; hcard3 = 8 })
        (best_player_hand board1 player_hand2) );
    ( "best_player_hand test 3: two pair" >:: fun _ ->
      assert_equal
        (Two_Pair { pair1 = 5; pair2 = 2; hcard = 14 })
        (best_player_hand board1 player_hand3) );
    ( "best_player_hand test 4: three of a kind" >:: fun _ ->
      assert_equal
        (Three_Of_A_Kind { three_kind = 6; hcard1 = 14; hcard2 = 12 })
        (best_player_hand board1 player_hand4) );
    ( "best_player_hand test 5: straight" >:: fun _ ->
      assert_equal
        (Straight { hcard = 6 })
        (best_player_hand board1 player_hand5) );
    ( "best_player_hand test 6: flush" >:: fun _ ->
      assert_equal
        (Flush { hcard1 = 14; hcard2 = 6; hcard3 = 5; hcard4 = 3; hcard5 = 2 })
        (best_player_hand board1 player_hand6) );
    ( "best_player_hand test 7: full house" >:: fun _ ->
      assert_equal
        (Full_House { three_kind = 11; pair = 12 })
        (best_player_hand board2 player_hand7) );
    ( "best_player_hand test 8: four of a kind" >:: fun _ ->
      assert_equal
        (Four_Of_A_Kind { four_kind = 6; hcard = 5 })
        (best_player_hand board3 player_hand4) );
    ( "best_player_hand test 9: straight flush" >:: fun _ ->
      assert_equal
        (Straight_Flush { hcard = 6 })
        (best_player_hand board1 player_hand8) );
    ( "best_player_hand test 10: royal flush" >:: fun _ ->
      assert_equal Royal_Flush (best_player_hand royal_flush player_hand4) );
  ]

let player1 = { name = "Player1"; hand = player_hand1; chips = 100 }
let player2 = { name = "Player2"; hand = player_hand2; chips = 100 }
let player3 = { name = "Player3"; hand = player_hand3; chips = 100 }
let player4 = { name = "Player4"; hand = player_hand4; chips = 100 }
let player5 = { name = "Player5"; hand = player_hand5; chips = 100 }
let player6 = { name = "Player6"; hand = player_hand6; chips = 100 }
let player7 = { name = "Player7"; hand = player_hand7; chips = 100 }
let player8 = { name = "Player8"; hand = player_hand8; chips = 100 }
let player9 = { name = "Player9"; hand = player_hand9; chips = 100 }
let player10 = { name = "Player10"; hand = player_hand10; chips = 100 }
let player11 = { name = "Player11"; hand = player_hand11; chips = 100 }
let player12 = { name = "Player12"; hand = player_hand12; chips = 100 }
let player13 = { name = "Player13"; hand = player_hand13; chips = 100 }
let player14 = { name = "Player14"; hand = player_hand14; chips = 100 }
let player15 = { name = "Player15"; hand = player_hand15; chips = 100 }

let best_hand_tests =
  [
    ( "best hand test 1: better high card" >:: fun _ ->
      assert_equal (Some player7) (best_hand board3 player1 player7) );
    ( "best hand test 2: better high card input switch" >:: fun _ ->
      assert_equal (Some player7) (best_hand board3 player7 player1) );
    ( "best hand test 3: equal high card" >:: fun _ ->
      assert_equal None (best_hand board4 player3 player6) );
    ( "best hand test 4: pair vs high card" >:: fun _ ->
      assert_equal (Some player2) (best_hand board1 player2 player1) );
    ( "best hand test 5: better pair" >:: fun _ ->
      assert_equal (Some player1) (best_hand board4 player1 player2) );
    ( "best hand test 6: equal pair, no tie" >:: fun _ ->
      assert_equal (Some player7) (best_hand board3 player1 player7) );
    ( "best hand test 8: equal pair, tie" >:: fun _ ->
      assert_equal None (best_hand board4 player2 player9) );
    ( "best hand test 9: pair vs two pair" >:: fun _ ->
      assert_equal (Some player3) (best_hand board1 player2 player3) );
    ( "best hand test 10: better two pair" >:: fun _ ->
      assert_equal (Some player10) (best_hand board1 player10 player3) );
    ( "best hand test 11: equal two pair, no tie" >:: fun _ ->
      assert_equal (Some player2) (best_hand board5 player2 player3) );
    ( "best hand test 12: equal two pair, tie" >:: fun _ ->
      assert_equal None (best_hand board5 player3 player11) );
    ( "best hand test 13: two pair vs three of a kind" >:: fun _ ->
      assert_equal (Some player4) (best_hand board1 player3 player4) );
    ( "best hand test 14: better three pair" >:: fun _ ->
      assert_equal (Some player4) (best_hand board1 player4 player12) );
    ( "best hand test 15: equal three pair, no tie" >:: fun _ ->
      assert_equal (Some player7) (best_hand board6 player2 player7) );
    ( "best hand test 16: equal three pair, tie" >:: fun _ ->
      assert_equal None (best_hand board6 player2 player9) );
    ( "best hand test 17: three of a kind vs straight" >:: fun _ ->
      assert_equal (Some player5) (best_hand board1 player4 player5) );
    ( "best hand test 18: better straight" >:: fun _ ->
      assert_equal (Some player7) (best_hand board7 player2 player7) );
    ( "best hand test 19: equal straight" >:: fun _ ->
      assert_equal None (best_hand board7 player14 player15) );
    ( "best hand test 20: straight vs flush" >:: fun _ ->
      assert_equal (Some player6) (best_hand board1 player5 player6) );
    ( "best hand test 21: better flush" >:: fun _ ->
      assert_equal (Some player7) (best_hand board1 player6 player7) );
    ( "best hand test 22: equal flush" >:: fun _ ->
      assert_equal None (best_hand board8 player8 player9) );
  ]

let suite =
  "test suite"
  >::: List.flatten
         [
           helper_tests;
           card_combinations_test;
           category_helper_tests;
           is_category_tests;
           hand_category_tests;
           best_player_hand_tests;
           best_hand_tests;
         ]

let _ = run_test_tt_main suite

(* DEBUGGING SPACE *)
