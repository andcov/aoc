open Core

type dir = R | L | D | U

let dir_of_string = function
  | "R" -> R
  | "L" -> L
  | "U" -> U
  | "D" -> D
  | _ -> failwith "wrong dir"

let read_input =
  In_channel.read_lines "input.in"
  |> List.map ~f:(fun s ->
         match String.split s ~on:' ' with
         | [ dir; num; _ ] -> (dir_of_string dir, Int.of_string num)
         | _ -> failwith "wrong input")

let size dirs =
  let n, _ =
    List.fold dirs ~init:(1, 1) ~f:(fun (max, sum) (dir, num, _) ->
        let sum' =
          match dir with R -> sum + num | L -> sum - num | _ -> sum
        in
        (Int.max max sum', sum'))
  in
  let m, _ =
    List.fold dirs ~init:(1, 1) ~f:(fun (max, sum) (dir, num, _) ->
        let sum' =
          match dir with D -> sum + num | U -> sum - num | _ -> sum
        in
        (Int.max max sum', sum'))
  in
  (m, n)

module Interval = struct
  (* inclusive interval: [l; r] *)
  type t = { l : int; r : int } [@@deriving sexp, show, eq]

  let compare i1 i2 = if i1.r < i2.l then -1 else if i1.l > i2.r then 1 else 0
  let ( >< ) a b = { l = Int.min a b; r = Int.max a b }
  let length { l; r } = r - l + 1

  let inter i1 i2 =
    if compare i1 i2 = 0 then Some (Int.max i1.l i2.l >< Int.min i1.r i2.r)
    else None
end

include Interval
module IntvSet = Set.Make (Interval)
module IntMap = Map.Make (Int)

let compl intvs int =
  let last_int = Set.max_elt_exn intvs in
  let init_set =
    if last_int.r = int.r then IntvSet.empty
    else IntvSet.singleton (last_int.r + 1 >< int.r)
  in
  Set.fold intvs ~init:(int.l, init_set) ~f:(fun (prvr, set) i ->
      let set' = if prvr = i.l then set else Set.add set (prvr >< i.l - 1) in
      (i.r + 1, set'))
  |> Tuple2.get2

let rec interval_map map dirs i j =
  match dirs with
  | [] -> map
  | (d, n) :: tl -> (
      match d with
      | U ->
          let map' = Map.add_multi map ~key:j ~data:(i - n >< i) in
          interval_map map' tl (i - n) j
      | D ->
          let map' = Map.add_multi map ~key:j ~data:(i >< i + n) in
          interval_map map' tl (i + n) j
      | L -> interval_map map tl i (j - n)
      | R -> interval_map map tl i (j + n))

let rec dig map x int is_inside =
  let rec next_x cx =
    match Map.closest_key map `Greater_than cx with
    | Some (nx, intvs) -> if Set.mem intvs int then (nx, intvs) else next_x nx
    | None -> Map.max_elt_exn map
  in
  let nx, nintvs = next_x x in
  if nx = x then 0
  else
    let nintvs = IntvSet.filter_map nintvs ~f:(fun i -> inter i int) in
    let compls = compl nintvs int in
    printf "Main int %s at %d is %s\n" (show int) x
      (if is_inside then "inside" else "outside");
    Set.iter nintvs ~f:(fun i -> printf "\tsame: %s at %d\n" (show i) nx);
    Set.iter compls ~f:(fun i -> printf "\tcompl: %s at %d\n" (show i) nx);
    print_endline "";
    Set.fold nintvs ~init:0 ~f:(fun acc s -> acc + dig map nx s (not is_inside))
    + Set.fold compls ~init:0 ~f:(fun acc s -> acc + dig map nx s is_inside)
(* let sum = if is_inside then (nx - x) * length int else 0 in *)

let () =
  let dirs = read_input in
  let map =
    interval_map IntMap.empty dirs 0 0
    |> Map.map ~f:(fun l -> IntvSet.of_list l)
  in
  let _ = dig map (-1) (0 >< 9) false in
  (* let m, n = size dirs in *)
  ()
