open Core

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

let int_of_char ch = Char.(to_int ch - to_int '0')

let read_input =
  In_channel.read_lines "input.in"
  |> List.map ~f:(fun l ->
         l |> String.to_list |> List.map ~f:int_of_char |> Array.of_list)
  |> Array.of_list

let size map = (Array.length map, Array.length map.(0))

type ori = V | H [@@deriving sexp, show, eq, ord, hash]

let rotate = function V -> H | H -> V

module Vertex = struct
  type t = int * int * ori [@@deriving sexp, show, eq, ord, hash]
end

module VMap = Map.Make (Vertex)

let rec get_cost map (r, c, ori) delta =
  match delta with
  | 0 -> 0
  | 1 | -1 -> (
      match ori with V -> map.(r).(c + delta) | H -> map.(r + delta).(c))
  | d when delta < 0 ->
      (match ori with V -> map.(r).(c + d) | H -> map.(r + d).(c))
      + get_cost map (r, c, ori) (d + 1)
  | d when delta > 0 ->
      (match ori with V -> map.(r).(c + d) | H -> map.(r + d).(c))
      + get_cost map (r, c, ori) (d - 1)
  | _ -> failwith "Unreachable"

let get_neighbours map (r, c, ori) =
  let m, n = size map in
  let delta =
    match ori with
    | V -> [ (0, -3); (0, -2); (0, -1); (0, 1); (0, 2); (0, 3) ]
    | H -> [ (-3, 0); (-2, 0); (-1, 0); (1, 0); (2, 0); (3, 0) ]
  in
  List.filter_map delta ~f:(fun (dr, dc) ->
      let r' = r + dr in
      let c' = c + dc in
      if r' >= 0 && r' < m && c' >= 0 && c' < n then
        Some ((r', c', rotate ori), get_cost map (r, c, ori) (dr + dc))
      else None)

let get_ultra_neighbours map (r, c, ori) =
  let m, n = size map in
  let deltas = (-10 <-> -3) @ (4 <-> 11) in
  let delta =
    match ori with
    | V -> List.map deltas ~f:(fun d -> (0, d))
    | H -> List.map deltas ~f:(fun d -> (d, 0))
  in
  List.filter_map delta ~f:(fun (dr, dc) ->
      let r' = r + dr in
      let c' = c + dc in
      if r' >= 0 && r' < m && c' >= 0 && c' < n then
        Some ((r', c', rotate ori), get_cost map (r, c, ori) (dr + dc))
      else None)

let get_dist dists (r, c) =
  [ Hashtbl.find dists (r, c, V); Hashtbl.find dists (r, c, H) ]
  |> List.filter_map ~f:Fn.id
  |> List.min_elt ~compare:Int.compare

let compare_v_cost (v1, c1) (v2, c2) =
  if c1 = c2 then Vertex.compare v1 v2 else Int.compare c1 c2

let dijkstra' map neigh_fun (sr, sc) e =
  let m, n = size map in
  let heap =
    Pairing_heap.create ~min_size:((m * n * 2) + 1) ~cmp:compare_v_cost ()
  in
  let dists = Hashtbl.create ~size:((m * n * 2) + 1) (module Vertex) in
  Pairing_heap.add heap ((sr, sc, V), 0);
  Pairing_heap.add heap ((sr, sc, H), 0);
  let rec relax () =
    match get_dist dists e with
    | Some d -> d
    | None -> (
        let curr_v, curr_cost = Pairing_heap.pop heap |> Option.value_exn in
        match Hashtbl.add dists ~key:curr_v ~data:curr_cost with
        | `Ok ->
            neigh_fun map curr_v
            |> List.filter ~f:(fun (n, _) -> Hashtbl.mem dists n |> not)
            |> List.iter ~f:(fun (n, c) ->
                   Pairing_heap.add heap (n, c + curr_cost));
            relax ()
        | `Duplicate -> relax ())
  in
  relax ()

let dijkstra map neigh_fun (sr, sc) e =
  try Some (dijkstra' map neigh_fun (sr, sc) e) with _ -> None

let () =
  let map = read_input in
  let m, n = size map in
  let c1 =
    dijkstra map get_neighbours (0, 0) (m - 1, n - 1) |> Option.value_exn
  in
  let c2 =
    dijkstra map get_ultra_neighbours (0, 0) (m - 1, n - 1) |> Option.value_exn
  in
  printf "Part 1: %d\nPart 2: %d\n" c1 c2
