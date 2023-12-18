open Core

type dir = R | L | D | U [@@deriving eq, show]

let dir_of_string = function
  | "R" -> R
  | "L" -> L
  | "U" -> U
  | "D" -> D
  | _ -> failwith "wrong dir"

let col_to_dir col =
  if Str.string_match (Str.regexp "(#\\([a-z0-9]+\\)\\([0-4]\\))") col 0 then
    let num = "0x" ^ Str.matched_group 1 col |> Int.of_string in
    let dir =
      match Str.matched_group 2 col with
      | "0" -> R
      | "1" -> D
      | "2" -> L
      | "3" -> U
      | _ -> failwith "wrongg format"
    in
    (dir, num)
  else failwith "wronggg format"

let read_input =
  In_channel.read_lines "input.in"
  |> List.map ~f:(fun s ->
         match String.split s ~on:' ' with
         | [ dir; num; col ] ->
             ((dir_of_string dir, Int.of_string num), col_to_dir col)
         | _ -> failwith "wrong input")

module Line = struct
  type pos = { x : int; y : int } [@@deriving sexp, show, eq]
  type ori = V | H [@@deriving sexp, show, eq]
  type t = { f : pos; t : pos; o : ori } [@@deriving sexp, show, eq]

  let create ~f:(fx, fy) ~t:(tx, ty) =
    let ori =
      if fx = tx then V
      else if fy = ty then H
      else failwith "line is not straight"
    in
    { f = { x = fx; y = fy }; t = { x = tx; y = ty }; o = ori }
end

include Line

let det p1 p2 = (p1.x * p2.y) - (p1.y * p2.x)

let solve dirs =
  let lines =
    List.fold dirs
      ~init:((0, 0), [])
      ~f:(fun ((px, py), lines) (dir, num) ->
        let npos =
          match dir with
          | U -> (px, py - num)
          | D -> (px, py + num)
          | R -> (px + num, py)
          | L -> (px - num, py)
        in
        let nline = create ~f:(px, py) ~t:npos in
        (npos, nline :: lines))
    |> Tuple2.get2 |> List.rev |> Array.of_list
  in
  let firstl =
    if equal_ori lines.(0).o V then (0, lines.(0)) else (1, lines.(1))
  in
  let si, _ =
    Array.foldi lines ~init:firstl ~f:(fun i (ai, acc) l ->
        if equal_ori l.o H then (ai, acc)
        else if acc.t.x < l.t.x then (i, l)
        else (ai, acc))
  in
  let n = Array.length lines in
  let co = ref R in
  let points = Array.create ~len:n { x = 0; y = 0 } in
  for di = 0 to n - 1 do
    let ci = (si + di) mod n in
    let ni = (si + di + 1) mod n in
    let commx, commy = (lines.(ci).t.x, lines.(ci).t.y) in
    let is_n_left = lines.(ni).f.x > lines.(ni).t.x in
    let is_n_up = lines.(ni).f.y > lines.(ni).t.y in
    let p =
      match !co with
      | R ->
          if is_n_left then (
            co := D;
            { x = commx + 1; y = commy + 1 })
          else (
            co := U;
            { x = commx + 1; y = commy })
      | L ->
          if is_n_left then (
            co := D;
            { x = commx; y = commy + 1 })
          else (
            co := U;
            { x = commx; y = commy })
      | U ->
          if is_n_up then (
            co := L;
            { x = commx; y = commy })
          else (
            co := R;
            { x = commx + 1; y = commy })
      | D ->
          if is_n_up then (
            co := L;
            { x = commx; y = commy + 1 })
          else (
            co := R;
            { x = commx + 1; y = commy + 1 })
    in
    points.(di) <- p
  done;
  let sum = ref 0 in
  for i = 0 to n - 1 do
    let cp = points.(i) in
    let np = points.((i + 1) mod n) in
    sum := !sum + det cp np
  done;
  !sum / 2

let () =
  let dirs, col_dirs = List.unzip read_input in
  let s1 = solve dirs in
  let s2 = solve col_dirs in
  printf "Part 1: %d\nPart 2: %d\n" s1 s2;
  ()
