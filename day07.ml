open Core

let ( >> ) f g x = f (g x)

module Hand = struct
  type typ = High | One | Two | Three | Full | Four | Five
  [@@deriving enum, show]

  type t = { cards : char list; typ : typ } [@@deriving show]

  let compare_typ typ1 typ2 = Int.compare (typ_to_enum typ1) (typ_to_enum typ2)
  let char_to_digit ch = Char.to_int ch - Char.to_int '0'

  let card_to_int = function
    | 'j' -> 1
    | '2' .. '9' as c -> char_to_digit c
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "wrong input"

  let compare_cards c1 c2 = Int.compare (card_to_int c1) (card_to_int c2)

  let compare_cards_with_joker c1 c2 =
    let c1 = if Char.equal c1 'J' then 'j' else c1 in
    let c2 = if Char.equal c2 'J' then 'j' else c2 in
    Int.compare (card_to_int c1) (card_to_int c2)

  let compare ?(compare_cards_fn = compare_cards) h1 h2 =
    let typ_cmp = compare_typ h1.typ h2.typ in
    if typ_cmp <> 0 then typ_cmp
    else
      let rec cmp_ch cards1 cards2 =
        match (cards1, cards2) with
        | c1 :: t1, c2 :: t2 ->
            let c_cmp = compare_cards_fn c1 c2 in
            if c_cmp <> 0 then c_cmp else cmp_ch t1 t2
        | _ -> 0
      in
      cmp_ch h1.cards h2.cards

  let of_string str =
    if String.length str <> 5 then failwith "wrong input"
    else
      let cards = String.to_list str in
      let grouped_cards =
        List.sort_and_group ~compare:compare_cards cards
        |> List.sort ~compare:(fun l1 l2 ->
               -1 * Int.compare (List.length l1) (List.length l2))
      in
      match grouped_cards with
      | [ _ ] -> { cards; typ = Five }
      | [ _; [ _ ] ] -> { cards; typ = Four }
      | [ [ _; _; _ ]; [ _; _ ] ] -> { cards; typ = Full }
      | [ [ _; _; _ ]; [ _ ]; [ _ ] ] -> { cards; typ = Three }
      | [ [ _; _ ]; [ _; _ ]; [ _ ] ] -> { cards; typ = Two }
      | [ [ _; _ ]; _; _; _ ] -> { cards; typ = One }
      | [ _; _; _; _; _ ] -> { cards; typ = High }
      | _ -> failwith "wrong format"

  let of_string_with_joker str =
    if String.length str <> 5 then failwith "wrong input"
    else
      let normal_hand = of_string str in
      if String.contains str 'J' then
        let cards = String.to_list str in
        let { cards = _; typ = winning_typ } =
          cards
          |> List.filter ~f:(not >> Char.equal 'F')
          |> List.sort_and_group ~compare:Char.compare
          |> List.map ~f:List.hd_exn
          |> List.map ~f:(fun ch ->
                 String.tr ~target:'J' ~replacement:ch str |> of_string)
          |> List.max_elt ~compare |> Option.value_exn
        in
        { normal_hand with typ = winning_typ }
      else normal_hand
end

let read_input ?(of_string_fn = Hand.of_string) () =
  In_channel.read_lines "input.in"
  |> List.map ~f:(fun l ->
         match String.split l ~on:' ' with
         | [ card; num ] -> (of_string_fn card, Int.of_string num)
         | _ -> failwith "wrong input")

let () =
  let sum1 =
    read_input ()
    |> List.sort ~compare:(fun (h1, _) (h2, _) -> Hand.compare h1 h2)
    |> List.mapi ~f:(fun i (_, bid) -> (i + 1) * bid)
    |> List.sum (module Int) ~f:Fn.id
  in
  let sum2 =
    read_input ~of_string_fn:Hand.of_string_with_joker ()
    |> List.sort ~compare:(fun (h1, _) (h2, _) ->
           Hand.compare ~compare_cards_fn:Hand.compare_cards_with_joker h1 h2)
    |> List.mapi ~f:(fun i (_, bid) -> (i + 1) * bid)
    |> List.sum (module Int) ~f:Fn.id
  in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
