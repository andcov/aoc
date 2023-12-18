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
         | [ dir; num; col ] -> (dir_of_string dir, Int.of_string num, col)
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

module Pos = struct
  type t = int * int [@@deriving sexp, hash, show, eq, ord]
end

let empty_neighbours tbl (i, j) =
  [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
  |> List.filter_map ~f:(fun (di, dj) ->
         if Hashtbl.mem tbl (i + di, j + dj) |> not then Some (i + di, j + dj)
         else None)

let rec fill tbl q =
  match q with
  | [] -> ()
  | pos :: tl ->
      if Hashtbl.mem tbl pos |> not then (
        Hashtbl.add_exn tbl ~key:pos ~data:"";
        let neigh = empty_neighbours tbl pos in
        fill tbl (neigh @ tl))
      else fill tbl tl

let rec outline tbl dirs (i, j) =
  match dirs with
  | [] -> ()
  | (dir, num, col) :: tl -> (
      match dir with
      | U ->
          for di = 1 to num do
            Hashtbl.add_exn tbl ~key:(i - di, j) ~data:col
          done;
          outline tbl tl (i - num, j)
      | D ->
          for di = 1 to num do
            Hashtbl.add_exn tbl ~key:(i + di, j) ~data:col
          done;
          outline tbl tl (i + num, j)
      | R ->
          for dj = 1 to num do
            Hashtbl.add_exn tbl ~key:(i, j + dj) ~data:col
          done;
          outline tbl tl (i, j + num)
      | L ->
          for dj = 1 to num do
            Hashtbl.add_exn tbl ~key:(i, j - dj) ~data:col
          done;
          outline tbl tl (i, j - num))

let () =
  let dirs = read_input in
  (* let m, n = size dirs in *)
  let size = 1 + List.fold dirs ~init:0 ~f:(fun acc (_, n, _) -> acc + n) in
  let tbl = Hashtbl.create ~size ~growth_allowed:false (module Pos) in
  outline tbl dirs (0, 0);
  fill tbl [ (1, 1) ];
  (* Array.iter map ~f:(fun r -> *)
  (*     Array.iter r ~f:(fun b -> if b then printf "#" else printf "."); *)
  (*     print_endline ""); *)
  printf "%d\n" (Hashtbl.length tbl)
