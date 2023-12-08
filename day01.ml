open Core

let find_digit_forward l only_nums =
  let digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in
  let length = String.length l in
  List.foldi
    ~init:(0, length + 1)
    digits
    ~f:(fun i (last_val, last_pos) digit ->
      try
        let regex =
          Str.regexp
          @@
          if only_nums then Int.to_string (i + 1)
          else digit ^ "\\|" ^ Int.to_string (i + 1)
        in
        let pos = Str.search_forward regex l 0 in
        if pos < last_pos then (i + 1, pos) else (last_val, last_pos)
      with _ -> (last_val, last_pos))

let find_digit_backward l only_nums =
  let digits =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in
  let length = String.length l in
  List.foldi ~init:(0, -1) digits ~f:(fun i (last_val, last_pos) digit ->
      try
        let regex =
          Str.regexp
          @@
          if only_nums then Int.to_string (i + 1)
          else digit ^ "\\|" ^ Int.to_string (i + 1)
        in
        let pos = Str.search_backward regex l length in
        if pos > last_pos then (i + 1, pos) else (last_val, last_pos)
      with _ -> (last_val, last_pos))

let get_num l ~only_nums =
  let valuef, _ = find_digit_forward l only_nums in
  let valueb, _ = find_digit_backward l only_nums in
  (valuef * 10) + valueb

let () =
  let lines = In_channel.read_lines "input.in" in
  let sum1 =
    List.map lines ~f:(get_num ~only_nums:true) |> List.fold ~init:0 ~f:( + )
  in
  let sum2 =
    List.map lines ~f:(get_num ~only_nums:false) |> List.fold ~init:0 ~f:( + )
  in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
