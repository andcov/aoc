open Core

let read_input =
  In_channel.read_lines "input.in"
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.map ~f:Int.of_string)

let rec is_same_el = function
  | [] | [ _ ] -> true
  | x :: y :: tl -> if x <> y then false else is_same_el (y :: tl)

let rec get_next_and_last_step (history : int list) : int * int =
  if is_same_el history then
    let hd = List.hd_exn history in
    (hd, hd)
  else
    let tl = List.tl_exn history in
    let zipped, _ = List.zip_with_remainder tl history in
    let diff_list = zipped |> List.map ~f:(fun (a, b) -> a - b) in
    let prev_first, prev_last = get_next_and_last_step diff_list in
    (List.hd_exn history - prev_first, List.last_exn history + prev_last)

let () =
  let histories = read_input in
  let sum_first, sum_last =
    histories
    |> List.map ~f:get_next_and_last_step
    |> List.fold ~init:(0, 0) ~f:(fun (acc_f, acc_l) (f, l) ->
           (acc_f + f, acc_l + l))
  in
  printf "Part 1: %d\nPart 2: %d\n" sum_last sum_first
