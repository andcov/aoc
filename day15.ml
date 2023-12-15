open Core

let read_input =
  In_channel.read_lines "input.in" |> List.hd_exn |> String.split ~on:','

let hash str =
  String.to_list str |> List.map ~f:Char.to_int
  |> List.fold ~init:0 ~f:(fun acc ch -> (acc + ch) * 17 mod 256)

module Hashmap = struct
  type t = (string * int) list Array.t

  let create : t = Array.create ~len:256 []

  let change (hashmap : t) ~lbl ~lens
      ~(map : string * int -> (string * int) Option.t) =
    let hash = hash lbl in
    let list = hashmap.(hash) in
    let is_pres =
      List.mem list (lbl, 0) ~equal:(fun (l1, _) (l2, _) -> String.equal l1 l2)
    in
    let new_list =
      if not is_pres then
        match map (lbl, lens) with None -> list | Some e -> e :: list
      else
        List.fold_right list ~init:[] ~f:(fun (plbl, plns) acc ->
            let new_el =
              if String.equal plbl lbl then map (lbl, lens)
              else Some (plbl, plns)
            in
            match new_el with None -> acc | Some el -> el :: acc)
    in
    hashmap.(hash) <- new_list

  let add hashmap ~lbl ~lens = change hashmap ~lbl ~lens ~map:(fun e -> Some e)
  let remove hashmap ~lbl = change hashmap ~lbl ~lens:0 ~map:(fun _ -> None)

  let focusing_power hashmap =
    Array.foldi hashmap ~init:0 ~f:(fun box sum l ->
        sum
        + (l |> List.rev
          |> List.foldi ~init:0 ~f:(fun slot sum' (_, lens) ->
                 sum' + ((box + 1) * (slot + 1) * lens))))
end

let apply_cmds hashmap cmds =
  List.iter cmds ~f:(fun cmd ->
      if String.mem cmd '-' then
        let lbl = String.split cmd ~on:'-' |> List.hd_exn in
        Hashmap.remove hashmap ~lbl
      else
        match String.split cmd ~on:'=' with
        | [ lbl; lens ] ->
            let lens = Int.of_string lens in
            Hashmap.add hashmap ~lbl ~lens
        | _ -> ())

let () =
  let cmds = read_input in
  let s1 = List.map cmds ~f:hash |> List.fold ~init:0 ~f:( + ) in
  let m = Hashmap.create in
  apply_cmds m cmds;
  printf "Part 1: %d\nPart 2: %d\n" s1 (Hashmap.focusing_power m)
