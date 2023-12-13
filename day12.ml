open Core

let read_lines =
  In_channel.read_lines "input.in"
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(function
       | [ line; nums ] ->
           (line, nums |> String.split ~on:',' |> List.map ~f:Int.of_string)
       | _ -> failwith "wrong input")

let five_lines =
  List.map ~f:(fun (line, nums) ->
      ( line ^ "?" ^ line ^ "?" ^ line ^ "?" ^ line ^ "?" ^ line,
        nums @ nums @ nums @ nums @ nums ))

let can_match string num =
  String.length string > num
  && (Char.equal '.' (String.nget string num)
     || Char.equal '?' (String.nget string num))
  && String.slice string 0 num
     |> String.for_all ~f:(fun ch -> Char.equal ch '?' || Char.equal ch '#')

let dynamic line nums =
  let line = line ^ "." in
  let nums = Array.of_list (nums @ [ 0 ]) |> Array.rev in
  let m = Array.length nums in
  let n = String.length line in
  let table = Array.make_matrix ~dimx:m ~dimy:(n + 1) 0 in

  let j = ref (n - 1) in
  while
    !j >= 0
    && String.nget line !j |> fun ch -> Char.equal ch '.' || Char.equal ch '?'
  do
    table.(0).(!j + 1) <- 1;
    j := !j - 1
  done;

  let dot i j = table.(i).(j + 1) in
  let hash i j =
    let slice = String.slice line j n in
    if can_match slice nums.(i) then table.(i - 1).(j + nums.(i) + 1) else 0
  in
  for i = 1 to m - 1 do
    for j = n - 2 downto 0 do
      match String.nget line j with
      | '.' -> table.(i).(j) <- dot i j
      | '#' -> table.(i).(j) <- hash i j
      | '?' -> table.(i).(j) <- hash i j + dot i j
      | _ -> failwith "wrong input"
    done
  done;
  table.(m - 1).(0)

let () =
  let lines = read_lines in
  let process =
    List.fold ~init:0 ~f:(fun acc (line, nums) -> acc + dynamic line nums)
  in
  let sum1 = process lines in
  let sum2 = five_lines lines |> process in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
