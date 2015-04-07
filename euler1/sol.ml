(* open Core.Std *)

let rec range a b =
    if a > b then
        []
    else
        a :: range (a + 1) b

let printList li =
    print_string ((String.concat " " (List.map string_of_int li)) ^ "\n")

let rec sum li =
    match li with
    | [] -> 0
    | el :: li -> el + (sum li)

let rec sum_multiples2 a b li =
    match li with
    | [] -> 0
    | el :: le -> 
            if (el mod 3) = 0 then el + (sum_multiples2 a b le)
            else if (el mod 5) = 0 then el + (sum_multiples2 a b le)
            else (sum_multiples2 a b le)

let () =
    Printf.printf "Hei\n";;
    printList (range 1 10);;
    print_int (sum (range 1 10));;

    print_int (sum_multiples2 3 5 (range 1 999));;
    print_char '\n'
