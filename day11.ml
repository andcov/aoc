open Core

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

let parse_input =
  let lines = In_channel.read_lines "input.in" in
  let m, n = (List.length lines, List.nth_exn lines 0 |> String.length) in
  let stars =
    lines
    |> List.mapi ~f:(fun row_i row ->
           String.to_list row
           |> List.filter_mapi ~f:(fun col_i ch ->
                  if Char.equal '#' ch then Some (row_i, col_i) else None))
    |> List.fold ~init:[] ~f:( @ )
  in
  let rows_set = 0 <-> m |> Set.of_list (module Int) in
  let empty_rows =
    List.fold stars ~init:rows_set ~f:(fun acc (s_row, _) ->
        Set.remove acc s_row)
  in
  let cols_set = 0 <-> n |> Set.of_list (module Int) in
  let empty_cols =
    List.fold stars ~init:cols_set ~f:(fun acc (_, s_col) ->
        Set.remove acc s_col)
  in
  (stars, empty_rows, empty_cols)

let dists stars empty_rows empty_cols dist_coeff =
  let dist (sr, sc) (er, ec) =
    let intersecting_r =
      Int.min sr er <-> Int.max sr er
      |> List.fold ~init:0 ~f:(fun acc r ->
             acc + if Set.mem empty_rows r then 1 else 0)
    in
    let intersecting_c =
      Int.min sc ec <-> Int.max sc ec
      |> List.fold ~init:0 ~f:(fun acc c ->
             acc + if Set.mem empty_cols c then 1 else 0)
    in
    ((intersecting_r + intersecting_c) * dist_coeff)
    + Int.abs (sr - er)
    - intersecting_r
    + Int.abs (sc - ec)
    - intersecting_c
  in
  let rec dist_to_star = function
    | start :: rest -> List.map rest ~f:(dist start) @ dist_to_star rest
    | _ -> []
  in
  dist_to_star stars

let () =
  let stars, empty_rows, empty_cols = parse_input in
  let solve coeff =
    dists stars empty_rows empty_cols coeff |> List.fold ~init:0 ~f:( + )
  in
  let sum1 = solve 2 in
  let sum2 = solve 1_000_000 in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
