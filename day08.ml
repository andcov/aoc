open Core

let ( >> ) f g x = f (g x)

let node_of_str str =
  let _first =
    Str.search_forward
      (Str.regexp {|\([A-Z0-9]+\) = (\([A-Z0-9]+\), \([A-Z0-9]+\))|})
      str 0
  in
  let node = Str.matched_group 1 str in
  let left = Str.matched_group 2 str in
  let right = Str.matched_group 3 str in
  (node, (left, right))

let read_input =
  let lines =
    In_channel.read_lines "input.in" |> List.filter ~f:(not >> String.is_empty)
  in
  match lines with
  | instr :: nodes ->
      let nodes = List.map nodes ~f:node_of_str in
      let instr = String.to_list instr in
      (instr, nodes)
  | _ -> failwith "wrong input"

let check_all_z = List.for_all ~f:(fun n -> String.nget n 2 |> Char.equal 'Z')

let find_steps_for_node instrs table start_node ~check_end =
  let current_node = ref start_node in
  let step_cnt = ref 0 in
  let _ =
    Sequence.find instrs ~f:(fun instr ->
        step_cnt := !step_cnt + 1;
        let left, right = Hashtbl.find_exn table !current_node in
        match (instr, left, right) with
        | 'L', left, _ when check_end left -> true
        | 'R', _, right when check_end right -> true
        | 'L', left, _ ->
            current_node := left;
            false
        | 'R', _, right ->
            current_node := right;
            false
        | _ -> failwith "unreachable")
  in
  !step_cnt

let rec gcd a b =
  if a mod b = 0 then b
  else if b = 0 then a
  else
    let r = a mod b in
    gcd b r

let lcm a b = a * (b / gcd a b)

let () =
  let instr, nodes = read_input in
  let a_nodes =
    nodes
    |> List.filter_map ~f:(fun (n, _) ->
           if String.nget n 2 |> Char.equal 'A' then Some n else None)
  in
  let table = Hashtbl.of_alist_exn (module String) nodes in
  let instrs = Sequence.cycle_list_exn instr in
  let steps1 =
    find_steps_for_node instrs table "AAA" ~check_end:(String.equal "ZZZ")
  in
  let periods =
    a_nodes
    |> List.map ~f:(fun node ->
           find_steps_for_node instrs table node ~check_end:(fun str ->
               String.nget str 2 |> Char.equal 'Z'))
  in
  let steps2 = List.fold periods ~init:1 ~f:(fun a b -> lcm a b) in
  (* let cnt2 = part2 instrs table a_nodes in *)
  printf "Part 1: %d\nPart 2: %d\n" steps1 steps2
