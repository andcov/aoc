open Core

let read_lines filename =
  In_channel.with_file filename ~f:(fun input ->
      In_channel.fold_lines input ~init:[] ~f:(fun l line ->
          if String.is_empty line then l else line :: l))
  |> List.rev

module Interval = struct
  type t = { l : int; r : int } [@@deriving sexp, show]

  let create_with_bounds l r =
    if l > r then
      failwith "cannot create interval with left bound higher than right bound"
    else { l; r }

  let create_with_length start len = { l = start; r = start + len - 1 }
  let compare i1 i2 = if i1.r < i2.l then -1 else if i1.l > i2.r then 1 else 0

  let merge_intervals intervals =
    let merge_intervals' intervals =
      List.fold intervals ~init:(List.hd_exn intervals) ~f:(fun acc intv ->
          { l = Int.min acc.l intv.l; r = Int.max acc.r intv.r })
    in
    let compare' i1 i2 =
      if i1.r + 1 < i2.l then -1 else if i1.l - 1 > i2.r then 1 else 0
    in
    List.sort_and_group intervals ~compare:compare'
    |> List.map ~f:merge_intervals'
end

module Mapping = struct
  type t = { source : Interval.t; dest : Interval.t } [@@deriving sexp, show]

  let create source dest len =
    {
      source = Interval.create_with_length source len;
      dest = Interval.create_with_length dest len;
    }

  let mem mapping ~el = mapping.source.l <= el && mapping.source.r >= el

  let map mapping ~el =
    if mem mapping ~el then mapping.dest.l + (el - mapping.source.l)
    else failwith "cannot map element that is not in mapping"

  let map_interval mapping (intv : Interval.t) =
    if Interval.compare intv mapping.source <> 0 then
      failwith "cannot map interval that is not in mapping"
    else
      let l = Int.max intv.l mapping.source.l in
      let r = Int.min intv.r mapping.source.r in
      let l = map mapping ~el:l in
      let r = map mapping ~el:r in
      Interval.create_with_bounds l r

  let of_string str =
    let nums_str = String.strip str |> String.split ~on:' ' in
    let nums = List.map nums_str ~f:Int.of_string in
    match nums with
    | [ dest; source; len ] -> create source dest len
    | _ -> failwith "wrong mapping format"
end

let get_maps lines =
  let all_maps, _ =
    List.fold lines ~init:([], []) ~f:(fun (all_maps, map) l ->
        if Str.string_match (Str.regexp ".*map.*") l 0 then (map :: all_maps, [])
        else (all_maps, Mapping.of_string l :: map))
  in
  List.rev all_maps
  |> List.map ~f:(fun maps ->
         List.sort maps ~compare:(fun m1 m2 ->
             Interval.compare m1.source m2.source))

(* ============= Part 1 ============= *)
let get_seeds str =
  String.index str ':' |> Option.value ~default:0 |> String.drop_prefix str
  |> String.strip |> String.split ~on:' '
  |> List.fold_right ~init:[] ~f:(fun s acc ->
         try Int.of_string s :: acc with _ -> acc)

let transform el ~maps =
  match List.find maps ~f:(Mapping.mem ~el) with
  | Some map -> Mapping.map map ~el
  | None -> el

let solve_part_1 seeds all_maps =
  let final_dests =
    List.fold all_maps ~init:seeds ~f:(fun acc maps ->
        List.map acc ~f:(transform ~maps))
  in
  List.min_elt final_dests ~compare:Int.compare |> Option.value ~default:0

(* ============= Part 2 ============= *)
let get_seeds_intervals str =
  String.index str ':' |> Option.value ~default:0 |> String.drop_prefix str
  |> String.strip |> String.split ~on:' '
  |> List.fold_right ~init:[] ~f:(fun s acc ->
         try Int.of_string s :: acc with _ -> acc)
  |> List.chunks_of ~length:2
  |> List.map ~f:(fun l ->
         match l with
         | [ left; len ] -> Interval.create_with_length left len
         | _ -> failwith "wrong number of seeds")

let get_overlapping (maps : Mapping.t list) (intv : Interval.t) : Mapping.t list
    =
  List.filter maps ~f:(fun map -> Interval.compare intv map.source = 0)

let apply_mappings_to_interval (maps : Mapping.t list) (intv : Interval.t) :
    Interval.t list =
  if List.is_empty maps then [ intv ]
  else
    let first_map = List.hd_exn maps in
    let maps =
      if intv.l < first_map.source.l then
        let additionl_map =
          Mapping.create intv.l intv.l (first_map.source.l - intv.l)
        in
        additionl_map :: maps
      else maps
    in
    let rec apply_maps maps : Interval.t list =
      match maps with
      | m1 :: m2 :: tl ->
          let mapping1 = Mapping.map_interval m1 intv in
          if m1.source.r + 1 = m2.source.l then mapping1 :: apply_maps (m2 :: tl)
          else
            mapping1
            :: Interval.create_with_bounds (m1.source.r + 1) m2.source.l
            :: apply_maps (m2 :: tl)
      | m :: [] ->
          let mapping = Mapping.map_interval m intv in
          if m.source.r >= intv.r then [ mapping ]
          else [ mapping; Interval.create_with_bounds (m.source.r + 1) intv.r ]
      | _ -> failwith "unreachable"
    in
    apply_maps maps |> Interval.merge_intervals

let solve_part_2 seeds_intervals all_maps =
  let final_dests =
    List.fold all_maps ~init:seeds_intervals ~f:(fun acc maps ->
        List.fold acc ~init:[] ~f:(fun new_intvs seed_intv ->
            let important_maps = get_overlapping maps seed_intv in
            let res = apply_mappings_to_interval important_maps seed_intv in
            res @ new_intvs)
        |> Interval.merge_intervals)
    |> Interval.merge_intervals
  in
  let min_int = List.hd_exn final_dests in
  min_int.l

let () =
  let lines = read_lines "input.in" @ [ "map" ] in
  let seeds = get_seeds @@ List.hd_exn lines in
  let seeds_intervals = get_seeds_intervals @@ List.hd_exn lines in
  let all_maps = get_maps @@ List.tl_exn lines in
  let ans_1 = solve_part_1 seeds all_maps in
  let ans_2 = solve_part_2 seeds_intervals all_maps in
  printf "Part 1: %d\nPart 2: %d\n" ans_1 ans_2
