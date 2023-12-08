open Core

let read_input =
  let line_to_nums line =
    match String.split line ~on:':' with
    | [ _; nums_str ] ->
        String.split ~on:' ' nums_str
        |> List.fold_right ~init:[] ~f:(fun str acc ->
               try Int.of_string str :: acc with _ -> acc)
    | _ -> failwith "wrong format"
  in
  let lines = In_channel.read_lines "input.in" in
  match lines with
  | [ time_line; dist_line ] ->
      List.zip_exn (line_to_nums time_line) (line_to_nums dist_line)
  | _ -> failwith "wrong format"

let rec cnt_digits = function 0 -> 0 | n -> 1 + cnt_digits (n / 10)

let solve_eq t d_max =
  let t = Float.of_int t in
  let d_max = Float.of_int d_max in
  let delta = Float.sqrt ((t *. t) -. (4.0 *. d_max)) in
  ((t -. delta) /. 2.0, (t +. delta) /. 2.0)

let possibilities_of_race race =
  let t, d = race in
  let t1, t2 = solve_eq t d in
  let t1r = t1 |> Float.round_up in
  let t2r = t2 |> Float.round_down in
  let t1 = (if Float.equal t1r t1 then t1r +. 1.0 else t1r) |> Int.of_float in
  let t2 = (if Float.equal t2r t2 then t2r -. 1.0 else t2r) |> Int.of_float in
  t2 - t1 + 1

let races_to_race =
  List.fold ~init:(0, 0) ~f:(fun (rt, rd) (t, d) ->
      let t_factor = Int.pow 10 (cnt_digits t) in
      let d_factor = Int.pow 10 (cnt_digits d) in
      ((rt * t_factor) + t, (rd * d_factor) + d))

let () =
  let races = read_input in
  let prod1 =
    List.map races ~f:possibilities_of_race |> List.fold ~init:1 ~f:( * )
  in
  let final_race = races_to_race races in
  let possibilites = possibilities_of_race final_race in
  printf "Part 1: %d\nPart 2: %d\n" prod1 possibilites
