open Core

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

let ( <=> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s + 1) ~f:(fun n -> n + s)

let read_lines =
  let orig =
    In_channel.read_lines "input.in"
    |> List.group ~break:(fun l1 l2 -> String.is_empty l1 || String.is_empty l2)
    |> List.filter ~f:(fun l -> List.for_all l ~f:(String.equal "") |> not)
    |> List.map ~f:(fun l -> List.map l ~f:String.to_array |> Array.of_list)
  in
  let trans =
    List.fold_right orig ~init:[] ~f:(fun ori acc ->
        let n, m = (Array.length ori, Array.length ori.(0)) in
        let tran = Array.make_matrix ~dimx:m ~dimy:n '.' in
        for i = 0 to n - 1 do
          for j = 0 to m - 1 do
            tran.(j).(i) <- ori.(i).(j)
          done
        done;
        tran :: acc)
  in
  List.zip_exn orig trans

let num_mistakes a1 a2 =
  Array.zip_exn a1 a2
  |> Array.count ~f:(fun (c1, c2) -> Char.equal c1 c2 |> not)

let find_adj_eq mat =
  0
  <-> Array.length mat - 1
  |> List.fold_right ~init:[] ~f:(fun i acc ->
         if num_mistakes mat.(i) mat.(i + 1) = 0 then i :: acc else acc)

let is_sim mat i =
  let n = Array.length mat in
  let lim = Int.min (i + 1) (n - i - 1) in
  1 <=> lim
  |> List.for_all ~f:(fun di -> num_mistakes mat.(i - di + 1) mat.(i + di) = 0)

let find_adj_eq_or_one_mis mat =
  0
  <-> Array.length mat - 1
  |> List.fold_right ~init:[] ~f:(fun i acc ->
         if num_mistakes mat.(i) mat.(i + 1) <= 1 then i :: acc else acc)

let is_sim_with_one_mistake mat axis =
  let n = Array.length mat in
  let rec check i num_mis =
    if num_mis > 1 then false
    else if axis - i + 1 < 0 || axis + i >= n then num_mis = 1
    else
      let curr_mis = num_mistakes mat.(axis - i + 1) mat.(axis + i) in
      check (i + 1) (num_mis + curr_mis)
  in
  check 1 0

let solve groups ~find_fn ~sim_fn =
  List.fold ~init:0 groups ~f:(fun acc (a, b) ->
      let eq_rows = find_fn a in
      let horiz =
        match List.find eq_rows ~f:(sim_fn a) with None -> 0 | Some r -> r + 1
      in

      let eq_cols = find_fn b in
      let vert =
        match List.find eq_cols ~f:(sim_fn b) with None -> 0 | Some c -> c + 1
      in
      acc + vert + (100 * horiz))

let () =
  let groups = read_lines in
  let sum1 = solve groups ~find_fn:find_adj_eq ~sim_fn:is_sim in
  let sum2 =
    solve groups ~find_fn:find_adj_eq_or_one_mis ~sim_fn:is_sim_with_one_mistake
  in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
