

let row_b = "+---------+ +---------+ +---------+ +---------+ +---------+" in
let rown1 = "|10       | | J       | | Q       | | K       | | A       |" in 
let row_r = "|         | |         | |         | |         | |         |" in 
let row_s = "|    ♠    | |    ♠    | |    ♠    | |    ♠    | |    ♠    |" in 
let rown2 = "|       10| |       J | |       Q | |       K | |       A |" in
print_endline row_b;
print_endline rown1;
print_endline row_r;
print_endline row_s;
print_endline row_r;
print_endline rown2;
print_endline row_b;
print_newline ();
print_endline "Welcome to Poker!";
print_newline ();
print_endline "Here is the full deck!";

let open Poker in 
Poker.print_deck Poker.full_deck

