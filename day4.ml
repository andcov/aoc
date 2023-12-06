open Core

let read_lines filename =
  In_channel.with_file filename ~f:(fun input ->
      In_channel.fold_lines input ~init:[] ~f:(fun l line -> line :: l))
  |> List.rev

let ( << ) f g x = f (g x)

type card = { winning_nums : int list; nums : int list } [@@deriving show]

let get_int_list str =
  String.strip str |> String.split ~on:' '
  |> List.fold_right ~init:[] ~f:(fun s acc ->
         try Int.of_string s :: acc with _ -> acc)

let line_to_card line =
  match String.split ~on:':' line with
  | [ _; nums_str ] -> (
      match String.split ~on:'|' nums_str with
      | [ win_str; nums_str ] ->
          { winning_nums = get_int_list win_str; nums = get_int_list nums_str }
      | _ -> failwith "invalid input (no '|')")
  | _ -> failwith "invalid input (no ':')"

let binary_search n ~list:l =
  let arr = Array.of_list l in
  let len = Array.length arr in
  let rec binary_search' (s : int) (e : int) : bool =
    if s >= e then equal arr.(s) n
    else
      let mij = (s + e) / 2 in
      equal arr.(mij) n
      ||
      if arr.(mij) > n then binary_search' s mij else binary_search' (mij + 1) e
  in
  if n < arr.(0) || n > arr.(len - 1) then false else binary_search' 0 len

let is_even n = n mod 2 = 0

let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative"
  else
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2)
    in
    aux 1 base exponent

let cnt_matching_nums card =
  List.count card.winning_nums ~f:(binary_search ~list:card.nums)

let rec process counts cards =
  match (counts, cards) with
  | [], [] -> []
  | cnt :: cnts, crd :: crds ->
      let matches = cnt_matching_nums crd in
      let cnts' =
        List.mapi cnts ~f:(fun i x -> if i < matches then x + cnt else x)
      in

      cnt :: process cnts' crds
  | _ -> failwith "counts and cards do not match in length"

let () =
  let lines = read_lines "input.in" in
  let cards = List.map lines ~f:line_to_card in
  let cards =
    List.map cards ~f:(fun card ->
        { card with nums = List.sort card.nums ~compare:Int.compare })
  in
  let sum1 =
    List.filter_map cards ~f:(fun card ->
        let matches = cnt_matching_nums card in
        if matches = 0 then None else Some (pow 2 (matches - 1)))
    |> List.sum (module Int) ~f:Fn.id
  in
  let initial_cnts = List.init (List.length cards) ~f:(fun _ -> 1) in
  let final_cnts = process initial_cnts cards in
  let sum2 = List.sum (module Int) final_cnts ~f:Fn.id in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
