open Core

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

type cell = Empty | VSplit | HSplit | FSlash | BSlash
type dir = N | W | S | E [@@deriving show, sexp, eq, ord]

let is_h_dir = function W | E -> true | _ -> false
let is_v_dir = function S | N -> true | _ -> false

module Beam = struct
  type t = int * int * dir [@@deriving sexp, show, eq, ord]
end

module Pos = struct
  type t = int * int [@@deriving sexp, show, eq, ord]
end

let dit_to_str = function N -> "N" | S -> "S" | E -> "E" | W -> "W"

let cell_of_char = function
  | '.' -> Empty
  | '|' -> VSplit
  | '-' -> HSplit
  | '/' -> FSlash
  | '\\' -> BSlash
  | _ -> failwith "unreachable"

let read_input =
  In_channel.read_lines "input.in"
  |> List.map ~f:(fun s ->
         String.to_list s |> List.map ~f:cell_of_char |> Array.of_list)
  |> Array.of_list

let size map = (Array.length map, Array.length map.(0))

let validate map (r, c, d) =
  let m, n = size map in
  if r < 0 || r >= m || c < 0 || c >= n then None else Some (r, c, d)

let forward (r, c, d) =
  match d with
  | N -> (r - 1, c, d)
  | S -> (r + 1, c, d)
  | E -> (r, c + 1, d)
  | W -> (r, c - 1, d)

let jump_empty map (r, c, d) =
  let rec jump' acc (r, c) =
    match map.(r).(c) with
    | Empty -> (
        let next = forward (r, c, d) |> validate map in
        match next with
        | None -> (r, c, d) :: acc
        | Some (nr, nc, _) -> (r, c, d) :: jump' acc (nr, nc))
    | VSplit when is_v_dir d -> (
        let next = forward (r, c, d) |> validate map in
        match next with
        | None -> (r, c, d) :: acc
        | Some (nr, nc, _) -> (r, c, d) :: jump' acc (nr, nc))
    | HSplit when is_h_dir d -> (
        let next = forward (r, c, d) |> validate map in
        match next with
        | None -> (r, c, d) :: acc
        | Some (nr, nc, _) -> (r, c, d) :: jump' acc (nr, nc))
    | _ -> (r, c, d) :: acc
  in
  jump' [] (r, c)

let move_beam map pos =
  let r, c, d = pos in
  (match map.(r).(c) with
  | Empty -> [ forward pos ]
  | VSplit -> (
      match d with
      | N | S -> [ forward pos ]
      | E | W -> [ forward (r, c, N); forward (r, c, S) ])
  | HSplit -> (
      match d with
      | E | W -> [ forward pos ]
      | N | S -> [ forward (r, c, E); forward (r, c, W) ])
  | FSlash -> (
      match d with
      | N -> [ forward (r, c, E) ]
      | S -> [ forward (r, c, W) ]
      | E -> [ forward (r, c, N) ]
      | W -> [ forward (r, c, S) ])
  | BSlash -> (
      match d with
      | N -> [ forward (r, c, W) ]
      | S -> [ forward (r, c, E) ]
      | E -> [ forward (r, c, S) ]
      | W -> [ forward (r, c, N) ]))
  |> List.filter_map ~f:(validate map)

module BeamSet = Set.Make (Beam)
module PosSet = Set.Make (Pos)

let rec tick map beams visited =
  match beams with
  | [] -> visited
  | beams ->
      let visited', beams =
        List.fold_map beams ~init:visited ~f:(fun acc b ->
            let jump = jump_empty map b in
            let visited' = List.fold jump ~init:acc ~f:Set.add in
            let last_j = List.last_exn jump in
            (visited', last_j))
      in
      let beams' =
        beams
        |> List.filter ~f:(fun b -> Set.mem visited b |> not)
        |> List.fold ~init:[] ~f:(fun acc b -> move_beam map b @ acc)
      in
      tick map beams' visited'

let beamset_to_posset (set : BeamSet.t) =
  Set.fold set ~init:PosSet.empty ~f:(fun acc (r, c, _) -> Set.add acc (r, c))

let get_starts map =
  let m, n = size map in
  let top = 0 <-> n |> List.map ~f:(fun i -> (0, i, S)) in
  let bot = 0 <-> n |> List.map ~f:(fun i -> (m - 1, i, N)) in
  let left = 0 <-> m |> List.map ~f:(fun i -> (i, 0, E)) in
  let right = 0 <-> m |> List.map ~f:(fun i -> (i, n - 1, W)) in
  top @ bot @ left @ right

let () =
  let map = read_input in

  let s1 =
    tick map [ (0, 0, E) ] BeamSet.empty |> beamset_to_posset |> Set.length
  in
  let s2 =
    get_starts map
    |> List.map ~f:(fun s ->
           tick map [ s ] BeamSet.empty |> beamset_to_posset |> Set.length)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  printf "Part 1: %d\nPart 2: %d\n" s1 s2
