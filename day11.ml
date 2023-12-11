open Core

module Pos = struct
  type t = { row : int; col : int } [@@deriving eq, ord, show, sexp]

  let of_tuple (row, col) = { row; col }
end

include Pos
module PosSet = Set.Make (Pos)
module PosMap = Map.Make (Pos)

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

let read_stars =
  let lines = In_channel.read_lines "input.in" in
  let m, n = (List.length lines, List.nth_exn lines 0 |> String.length) in
  let stars_list =
    lines
    |> List.mapi ~f:(fun row_i row ->
           String.to_list row
           |> List.filter_mapi ~f:(fun col_i ch ->
                  if Char.equal '#' ch then Some (Pos.of_tuple (row_i, col_i))
                  else None))
    |> List.fold ~init:[] ~f:( @ )
  in
  (PosSet.of_list stars_list, m, n)

let empty_rc m n stars =
  let empty_rows =
    0 <-> m
    |> List.filter ~f:(fun row ->
           Set.exists stars ~f:(fun star -> star.row = row) |> not)
    |> Core.Set.of_list (module Int)
  in
  let empty_cols =
    0 <-> n
    |> List.filter ~f:(fun col ->
           Set.exists stars ~f:(fun star -> star.col = col) |> not)
    |> Set.of_list (module Int)
  in
  (empty_rows, empty_cols)

let get_neigh pos m n =
  [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
  |> List.filter_map ~f:(fun (dr, dc) ->
         let r' = pos.row + dr in
         let c' = pos.col + dc in
         if
           Int.between r' ~low:0 ~high:(m - 1)
           && Int.between c' ~low:0 ~high:(n - 1)
         then Some (Pos.of_tuple (r', c'))
         else None)

let get_dists stars start ends m n empty_offset =
  let empty_rows, empty_cols = empty_rc m n stars in
  let dist_offset pos =
    if Set.mem empty_rows pos.row || Set.mem empty_cols pos.col then
      empty_offset
    else 1
  in
  let q = Queue.of_list [ (start, 0) ] in
  let dists = PosMap.of_alist_exn [ (start, 0) ] in
  let rec bfs dists =
    if Queue.is_empty q then dists
    else
      let tpos, tdist = Queue.dequeue_exn q in
      let neighs = get_neigh tpos m n in
      let new_dists =
        List.fold neighs ~init:dists ~f:(fun acc neigh ->
            if Map.mem dists neigh |> not then (
              let new_dist = tdist + dist_offset neigh in
              Queue.enqueue q (neigh, new_dist);
              Map.add_exn acc ~key:neigh ~data:new_dist)
            else acc)
      in
      bfs new_dists
  in
  bfs dists |> Map.to_alist
  |> List.filter_map ~f:(fun (pos, dist) ->
         if List.mem ends pos ~equal:Pos.equal then Some (pos, dist) else None)

let get_all_dists stars m n empty_offset =
  let rec get_dists' = function
    | start :: ends ->
        let curr_dists =
          get_dists stars start ends m n empty_offset
          |> List.map ~f:(fun (pos, dist) -> (start, pos, dist))
        in
        curr_dists @ get_dists' ends
    | _ -> []
  in
  Set.to_list stars |> get_dists'

let () =
  let stars, m, n = read_stars in
  let sum1 =
    get_all_dists stars m n 2
    |> List.fold ~init:0 ~f:(fun acc (_, _, dist) -> acc + dist)
  in
  let sum2 =
    get_all_dists stars m n 1000000
    |> List.fold ~init:0 ~f:(fun acc (_, _, dist) -> acc + dist)
  in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
