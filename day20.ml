open Core

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

module StrMap = Map.Make (String)

type module_typ = FF of bool | Conj of bool StrMap.t | Broad

let group_to_neigh str =
  Str.global_replace (Str.regexp " ") "" str |> String.split ~on:','

let read_input =
  let reg =
    Re.(
      seq
        [
          alt [ char '%'; char '&'; epsilon ];
          wordc |> rep1 |> group;
          str " -> ";
          any |> rep |> group;
        ]
      |> compile)
  in
  In_channel.read_lines "input.in"
  |> List.fold ~init:StrMap.empty ~f:(fun map l ->
         let grs = Re.exec reg l in
         let name = Re.Group.get grs 1 in
         let neigh = Re.Group.get grs 2 |> group_to_neigh in
         let typ =
           if String.mem l '%' then FF false
           else if String.mem l '&' then Conj StrMap.empty
           else Broad
         in
         Map.add_exn map ~key:name ~data:(typ, neigh))

let rec add_conj mlist map =
  match mlist with
  | [] -> map
  | (name, (_, neighs)) :: tl ->
      let map' =
        List.fold neighs ~init:map ~f:(fun acc neigh ->
            Map.change acc neigh ~f:(fun neigh_d ->
                match neigh_d with
                | None -> None
                | Some (ntyp, nneigh) -> (
                    match ntyp with
                    | Conj deps ->
                        let deps' = Map.add_exn deps ~key:name ~data:false in
                        Some (Conj deps', nneigh)
                    | _ -> Some (ntyp, nneigh))))
      in
      add_conj tl map'

let rec pulse q map =
  match Queue.dequeue q with
  | None -> (map, 1, 0)
  | Some (name, from, p) -> (
      match Map.find map name with
      | None -> pulse q map
      | Some m ->
          let map', low, high =
            match m with
            | Broad, neigh ->
                List.iter neigh ~f:(fun n ->
                    printf "%s -%b-> %s\n" name false n;
                    Queue.enqueue q (n, name, false));
                (map, List.length neigh, 0)
            | FF state, neigh ->
                if p then (map, 0, 0)
                else
                  let map' =
                    Map.change map name ~f:(fun _ ->
                        Some (FF (not state), neigh))
                  in
                  List.iter neigh ~f:(fun n ->
                      printf "%s -%b-> %s\n" name (not state) n;
                      Queue.enqueue q (n, name, not state));
                  let low_cnt = if state then 0 else List.length neigh in
                  (map', low_cnt, List.length neigh - low_cnt)
            | Conj deps, neigh ->
                let deps' = Map.change deps from ~f:(fun _ -> Some p) in
                let map' =
                  Map.change map name ~f:(fun _ -> Some (Conj deps', neigh))
                in
                let all_high = Map.for_all deps' ~f:Fn.id in
                List.iter neigh ~f:(fun n ->
                    printf "%s -%b-> %s\n" name (not all_high) n;
                    Queue.enqueue q (n, name, not all_high));
                let low_cnt = if all_high then List.length neigh else 0 in
                (map', low_cnt, List.length neigh - low_cnt)
          in
          let map', rec_low, rec_high = pulse q map' in
          (map', low + rec_low, high + rec_high))

let () =
  let map = read_input in
  let map = add_conj (Map.to_alist map) map in

  (* Map.iteri map ~f:(fun ~key ~data -> *)
  (*     printf "module %s: " key; *)
  (*     match data with *)
  (*     | Broad, neigh -> *)
  (*         printf "broad\n"; *)
  (*         List.iter neigh ~f:(printf "%s  "); *)
  (*         printf "\n\n" *)
  (*     | FF _, neigh -> *)
  (*         printf "flip flop\n"; *)
  (*         List.iter neigh ~f:(printf "%s  "); *)
  (*         printf "\n\n" *)
  (*     | Conj deps, neigh -> *)
  (*         printf "conj\nDeps: "; *)
  (*         Map.iter_keys deps ~f:(printf "%s  "); *)
  (*         printf "\nNeigh: "; *)
  (*         List.iter neigh ~f:(printf "%s  "); *)
  (*         printf "\n\n"); *)
  let _, l, h =
    0 <-> 1
    |> List.fold ~init:(map, 0, 0) ~f:(fun (m, l, h) _ ->
           let q = Queue.create () in
           Queue.enqueue q ("broadcaster", "", false);
           let m', l', h' = pulse q m in
           printf "\n\n";
           (m', l + l', h + h'))
  in
  (* 880928730 *)
  printf "Part 1: %d %d\n" l h;
  ()
