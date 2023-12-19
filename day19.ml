open Core
open Re

module Part = struct
  type t = { x : int; m : int; a : int; s : int } [@@deriving show]

  let get_field part = function
    | "x" -> part.x
    | "m" -> part.m
    | "a" -> part.a
    | "s" -> part.s
    | _ -> failwith "wrong field"

  let sum p = p.x + p.m + p.a + p.s

  let of_string str =
    let nums =
      [ "x"; "m"; "a"; "s" ]
      |> List.map ~f:(fun f ->
             let reg =
               Re.(seq [ str (f ^ "="); digit |> rep1 |> group ] |> compile)
             in
             Group.get (Re.exec reg str) 1 |> int_of_string)
    in
    {
      x = List.nth_exn nums 0;
      m = List.nth_exn nums 1;
      a = List.nth_exn nums 2;
      s = List.nth_exn nums 3;
    }
end

module IntervalPart = struct
  type intv = { l : int; r : int } [@@deriving show, sexp]
  type t = { x : intv; m : intv; a : intv; s : intv } [@@deriving show, sexp]

  let get_field part = function
    | "x" -> part.x
    | "m" -> part.m
    | "a" -> part.a
    | "s" -> part.s
    | _ -> failwith "wrong field"

  let i_of_tuple l r = { l = Int.min l r; r = Int.max l r }

  let full =
    let i = i_of_tuple 1 4000 in
    { x = i; m = i; a = i; s = i }

  let size i = i.r - i.l + 1
  let options ipart = size ipart.x * size ipart.m * size ipart.a * size ipart.s

  let inter i1 i2 =
    if i1.r < i2.l || i1.l > i2.r then None
    else Some (i_of_tuple (Int.max i1.l i2.l) (Int.min i1.r i2.r))
end

module Rule = struct
  type pred = True | Pred of string * (int -> int -> bool) * int
  [@@deriving show]

  type t = { dest : string; p : pred } [@@deriving show]

  let satisfies rule part =
    match rule.p with
    | True -> true
    | Pred (f, func, lim) -> func (Part.get_field part f) lim

  let relax rule (ipart : IntervalPart.t) =
    match rule.p with
    | True -> (Some ipart, None)
    | Pred (f, func, lim) -> (
        let good_interval =
          if phys_equal ( > ) func then IntervalPart.i_of_tuple (lim + 1) 4000
          else IntervalPart.i_of_tuple 1 (lim - 1)
        in
        let bad_interval =
          if phys_equal ( > ) func then IntervalPart.i_of_tuple 1 lim
          else IntervalPart.i_of_tuple lim 4000
        in
        match f with
        | "x" ->
            let good_x =
              match IntervalPart.inter good_interval ipart.x with
              | None -> None
              | Some i -> Some { ipart with x = i }
            in
            let bad_x =
              match IntervalPart.inter bad_interval ipart.x with
              | None -> None
              | Some i -> Some { ipart with x = i }
            in
            (good_x, bad_x)
        | "m" ->
            let good_m =
              match IntervalPart.inter good_interval ipart.m with
              | None -> None
              | Some i -> Some { ipart with m = i }
            in
            let bad_m =
              match IntervalPart.inter bad_interval ipart.m with
              | None -> None
              | Some i -> Some { ipart with m = i }
            in
            (good_m, bad_m)
        | "a" ->
            let good_a =
              match IntervalPart.inter good_interval ipart.a with
              | None -> None
              | Some i -> Some { ipart with a = i }
            in
            let bad_a =
              match IntervalPart.inter bad_interval ipart.a with
              | None -> None
              | Some i -> Some { ipart with a = i }
            in
            (good_a, bad_a)
        | "s" ->
            let good_s =
              match IntervalPart.inter good_interval ipart.s with
              | None -> None
              | Some i -> Some { ipart with s = i }
            in
            let bad_s =
              match IntervalPart.inter bad_interval ipart.s with
              | None -> None
              | Some i -> Some { ipart with s = i }
            in
            (good_s, bad_s)
        | _ -> failwith "wrong field")

  let of_string str =
    if String.mem str ':' then
      let f =
        Re.(alt [ char 'x'; char 'm'; char 'a'; char 's' ] |> rep1 |> group)
      in
      let sign = Re.(alt [ char '>'; char '<' ] |> group) in
      let num = Re.(digit |> rep1 |> group) in
      let dest = Re.(wordc |> rep1 |> group) in
      let reg = Re.(seq [ f; sign; num; char ':'; dest ] |> compile) in
      let gs = Re.exec reg str in

      let f = Group.get gs 1 in
      let sign = if String.equal (Group.get gs 2) ">" then ( > ) else ( < ) in
      let num = Group.get gs 3 |> int_of_string in
      let dest = Group.get gs 4 in

      { dest; p = Pred (f, sign, num) }
    else { dest = str; p = True }
end

let wkfl_of_string str =
  let reg =
    Re.(
      seq [ wordc |> rep1 |> group; str "{"; any |> rep |> group; str "}" ]
      |> compile)
  in
  let gs = Re.exec reg str in
  let name = Group.get gs 1 in
  let rules_str = Group.get gs 2 in
  let rules = String.split rules_str ~on:',' |> List.map ~f:Rule.of_string in
  (name, rules)

module WkflMap = Map.Make (String)

let parse_workflows wkfls =
  List.map wkfls ~f:wkfl_of_string |> WkflMap.of_alist_exn

let parse_parts parts = List.map parts ~f:Part.of_string

let read_input =
  let workflows, parts =
    In_channel.read_lines "input.in"
    |> List.split_while ~f:(fun s -> String.is_empty s |> not)
  in
  let parts = List.tl_exn parts in
  (parse_workflows workflows, parse_parts parts)

let rec send wkfls curr_wkfl part =
  match curr_wkfl with
  | "A" -> true
  | "R" -> false
  | _ ->
      let rules = Map.find_exn wkfls curr_wkfl in
      let check_rule =
        List.find_exn rules ~f:(fun r -> Rule.satisfies r part)
      in
      send wkfls check_rule.dest part

let rec send_interval wkfls curr_wkfl ipart =
  match curr_wkfl with
  | "A" -> IntervalPart.options ipart
  | "R" -> 0
  | _ ->
      let rules = Map.find_exn wkfls curr_wkfl in
      List.fold_until rules ~finish:Tuple2.get1 ~init:(0, ipart)
        ~f:(fun (sum, ipart) r ->
          let good, bad = Rule.relax r ipart in
          let delta =
            +(match good with
             | None -> 0
             | Some ip -> send_interval wkfls r.dest ip)
          in
          (* printf "with delta %d\n" delta; *)
          (* print_endline ""; *)
          match bad with
          | None -> Stop (sum + delta)
          | Some ip -> Continue (sum + delta, ip))

let () =
  let wkfls, parts = read_input in
  let sum1 =
    List.fold parts ~init:0 ~f:(fun acc p ->
        if send wkfls "in" p then acc + Part.sum p else acc)
  in
  let sum2 = send_interval wkfls "in" IntervalPart.full in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
