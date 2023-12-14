open Core

let read_lines =
  In_channel.read_lines "input.in"
  |> List.map ~f:String.to_array
  |> List.to_array

type dir = N | W | S | E

let tilt map o_pos dir =
  let n, m = (Array.length map, Array.length map.(0)) in

  let o_pos_len = ref 0 in

  let add_to_os i j =
    o_pos.(!o_pos_len) <- (i * m) + j;
    o_pos_len := !o_pos_len + 1
  in

  match dir with
  | E ->
      for i = 0 to n - 1 do
        let prev_avai = ref (m - 1) in
        for j = m - 1 downto 0 do
          match map.(i).(j) with
          | '#' -> prev_avai := j - 1
          | 'O' ->
              if !prev_avai = j then prev_avai := j - 1
              else (
                map.(i).(j) <- '.';
                map.(i).(!prev_avai) <- 'O';
                add_to_os i !prev_avai;
                prev_avai := !prev_avai - 1)
          | _ -> ()
        done
      done
  | W ->
      for i = 0 to n - 1 do
        let prev_avai = ref 0 in
        for j = 0 to m - 1 do
          match map.(i).(j) with
          | '#' -> prev_avai := j + 1
          | 'O' ->
              if !prev_avai = j then prev_avai := j + 1
              else (
                map.(i).(j) <- '.';
                map.(i).(!prev_avai) <- 'O';
                add_to_os i !prev_avai;
                prev_avai := !prev_avai + 1)
          | _ -> ()
        done
      done
  | N ->
      for j = 0 to m - 1 do
        let prev_avai = ref 0 in
        for i = 0 to n - 1 do
          match map.(i).(j) with
          | '#' -> prev_avai := i + 1
          | 'O' ->
              if !prev_avai = i then prev_avai := i + 1
              else (
                map.(i).(j) <- '.';
                map.(!prev_avai).(j) <- 'O';
                add_to_os !prev_avai j;
                prev_avai := !prev_avai + 1)
          | _ -> ()
        done
      done
  | S ->
      for j = 0 to m - 1 do
        let prev_avai = ref (n - 1) in
        for i = n - 1 downto 0 do
          match map.(i).(j) with
          | '#' -> prev_avai := i - 1
          | 'O' ->
              if !prev_avai = i then prev_avai := i - 1
              else (
                map.(i).(j) <- '.';
                map.(!prev_avai).(j) <- 'O';
                add_to_os !prev_avai j;
                prev_avai := !prev_avai - 1)
          | _ -> ()
        done
      done

let cycle map o_pos = [ N; W; S; E ] |> List.iter ~f:(tilt map o_pos)

let are_maps_eq o_pos1 o_pos2 =
  List.for_all o_pos1 ~f:(Array.mem o_pos2 ~equal:Int.equal)

let sum_map map =
  let m = Array.length map.(0) in
  Array.foldi map ~init:0 ~f:(fun i acc r ->
      acc + ((m - i) * Array.count r ~f:(Char.equal 'O')))

let create_o_pos map =
  let o_cnt =
    map
    |> Array.fold ~init:0 ~f:(fun acc r ->
           acc + Array.count r ~f:(Char.equal 'O'))
  in
  Array.create ~len:o_cnt 0

let solve1 map =
  tilt map (create_o_pos map) N;
  sum_map map

let solve2 map =
  let mem = Hashtbl.create (module Int) in

  let o_pos = create_o_pos map in
  let ce = ref 0 in
  let cs = ref 0 in
  let _, _ =
    () |> Sequence.repeat
    |> Sequence.drop_while_option ~f:(fun _ ->
           cycle map o_pos;
           ce := !ce + 1;

           let hash = Array.fold o_pos ~init:0 ~f:( + ) in
           match Hashtbl.add mem ~key:hash ~data:(!ce, Array.to_list o_pos) with
           | `Duplicate ->
               let same_id, same_hash = Hashtbl.find_exn mem hash in
               if are_maps_eq same_hash o_pos then (
                 cs := same_id;
                 false)
               else true
           | `Ok -> true)
    |> Option.value_exn
  in
  let m = 1000000000 in
  let ce = !ce - 1 in
  let cs = !cs in
  let cl = ce - cs + 1 in
  let r = (m - cs + 1) mod cl in
  let r = r - 1 in

  let inf_seq = Sequence.repeat () in
  Sequence.take inf_seq r |> Sequence.iter ~f:(fun _ -> cycle map o_pos);

  sum_map map

let () =
  let map = read_lines in
  printf "Part 1: %d\n" (solve1 map);
  let map = read_lines in
  printf "Part 2: %d\n" (solve2 map)
