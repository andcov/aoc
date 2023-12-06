open Core

let read_lines filename =
  In_channel.with_file filename ~f:(fun input ->
      In_channel.fold_lines input ~init:[] ~f:(fun l line ->
          if String.is_empty line then l else line :: l))
  |> List.rev

module Mapping = struct
  type t = { s_l : int; s_r : int; d_l : int; d_r : int }
  [@@deriving sexp, show]

  let create source dest len =
    { s_l = source; s_r = source + len - 1; d_l = dest; d_r = dest + len - 1 }

  let mem mapping ~el = mapping.s_l <= el && mapping.s_r >= el

  let map mapping ~el =
    if mem mapping ~el then mapping.d_l + (el - mapping.s_l)
    else failwith "cannot map element that is not in mapping"

  let compare m1 m2 =
    if m1.s_r < m2.s_l then -1
    else if m1.s_l > m2.s_r then 1
    else if m1.s_l = m2.s_l && m1.s_r = m2.s_r then 0
    else failwith "cannot compare overlapping intervals"

  let of_string str =
    let nums_str = String.strip str |> String.split ~on:' ' in
    let nums = List.map nums_str ~f:Int.of_string in
    match nums with
    | [ dest; source; len ] -> create source dest len
    | _ -> failwith "wrong mapping format"
end

(* module MappingsSet = struct *)
(*   type t = Mapping.t list *)
(**)
(*   let empty = [] *)
(**)
(*   let add (set : t) (map : Mapping.t) : t = *)
(*     List.fold_until set ~init:(empty, set) *)
(*       ~f:(fun (f_half, s_half) el -> *)
(*         if List.is_empty s_half then Stop (f_half @ [ el ]) *)
(*         else if Mapping.compare map el > 0 then *)
(*           Continue (f_half @ [ el ], List.tl_exn s_half) *)
(*         else Stop (f_half @ [ el ] @ s_half)) *)
(*       ~finish:(fun (f_half, s_half) -> f_half @ s_half) *)
(* end *)

let get_seeds str =
  String.index str ':' |> Option.value ~default:0 |> String.drop_prefix str
  |> String.strip |> String.split ~on:' '
  |> List.fold_right ~init:[] ~f:(fun s acc ->
         try Int.of_string s :: acc with _ -> acc)

let transform el ~maps =
  match List.find maps ~f:(Mapping.mem ~el) with
  | Some map -> Mapping.map map ~el
  | None -> el

let () =
  let lines = read_lines "input.in" @ [ "map" ] in
  let seeds = get_seeds @@ List.hd_exn lines in
  let all_maps, _ =
    List.tl_exn lines
    |> List.fold ~init:([], []) ~f:(fun (all_maps, map) l ->
           if Str.string_match (Str.regexp ".*map.*") l 0 then
             (map :: all_maps, [])
           else (all_maps, Mapping.of_string l :: map))
  in
  let all_maps = List.rev all_maps in
  let final_dests =
    List.fold all_maps ~init:seeds ~f:(fun acc maps ->
        List.map acc ~f:(transform ~maps))
  in
  let min_dest =
    List.min_elt final_dests ~compare:Int.compare |> Option.value ~default:0
  in
  printf "Part 1: %d\n" min_dest
