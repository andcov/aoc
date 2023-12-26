open Core
module SM = Map.Make (String)
module SS = Set.Make (String)

type ff_state = On | Off
type pulse_intens = Low | High

let show_intens = function Low -> "low" | High -> "high"

type module_typ =
  | FlipFlop of ff_state
  | Conj of pulse_intens SM.t
  | Broadcast
  | Endpoint

type pulse = { src : string; dest : string; intens : pulse_intens }

let add_conj map =
  let conjs =
    map |> Map.to_alist
    |> List.filter_map ~f:(fun (name, (typ, _)) ->
           match typ with Conj _ -> Some name | _ -> None)
  in
  List.fold conjs ~init:map ~f:(fun map' conj ->
      let conj_neighs =
        Map.filter_mapi map' ~f:(fun ~key:name ~data:(_, neighs) ->
            if List.mem neighs conj ~equal:String.equal then Some Low else None)
      in
      Map.update map' conj ~f:(fun option ->
          let _, neighs = Option.value_exn option in
          (Conj conj_neighs, neighs)))

let add_endpoints map =
  let all_neighbours =
    map
    |> Map.fold ~init:SS.empty ~f:(fun ~key:_ ~data:(_, neighbours) set ->
           SS.of_list neighbours |> Set.union set)
  in
  Set.fold all_neighbours ~init:map ~f:(fun map' neigh ->
      Map.change map' neigh ~f:(function
        | None -> Some (Endpoint, [])
        | Some v -> Some v))

let read_input =
  let group_to_neigh str =
    Str.global_replace (Str.regexp " ") "" str |> String.split ~on:','
  in
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
  let map =
    In_channel.read_lines "input.in"
    |> List.fold ~init:SM.empty ~f:(fun map l ->
           let grs = Re.exec reg l in
           let name = Re.Group.get grs 1 in
           let neigh = Re.Group.get grs 2 |> group_to_neigh in
           let typ =
             if String.mem l '%' then FlipFlop Off
             else if String.mem l '&' then Conj SM.empty
             else Broadcast
           in
           Map.add_exn map ~key:name ~data:(typ, neigh))
  in
  map |> add_endpoints |> add_conj

let process_pulse queue map =
  match queue with
  | [] -> ([], map)
  | p :: q -> (
      match Map.find_exn map p.dest with
      | Broadcast, neighs ->
          ( List.map neighs ~f:(fun neigh ->
                { src = p.dest; dest = neigh; intens = Low }),
            map )
      | FlipFlop state, neighs -> (
          match p.intens with
          | Low ->
              let state', intens' =
                match state with Off -> (On, High) | On -> (Off, Low)
              in
              ( q
                @ List.map neighs ~f:(fun neigh ->
                      { src = p.dest; dest = neigh; intens = intens' }),
                Map.update map p.dest ~f:(fun _ -> (FlipFlop state', neighs)) )
          | High -> (q, map))
      | Conj deps, neighs ->
          let deps' = Map.update deps p.src ~f:(fun _ -> p.intens) in
          let all_high =
            Map.for_all deps' ~f:(function High -> true | Low -> false)
          in
          let intens' = if all_high then Low else High in
          ( q
            @ List.map neighs ~f:(fun neigh ->
                  { src = p.dest; dest = neigh; intens = intens' }),
            Map.update map p.dest ~f:(fun _ -> (Conj deps', neighs)) )
      | Endpoint, _ -> (q, map))

let press_button map =
  let rec process_pulse' q map pulses =
    match q with
    | p :: q ->
        let q', map' = process_pulse (p :: q) map in
        process_pulse' q' map' (p :: pulses)
    | [] -> (pulses, map)
  in
  process_pulse'
    [ { src = "button"; dest = "broadcaster"; intens = Low } ]
    map []

let part1 map =
  Seq.unfold
    (fun map ->
      let pulses, map' = press_button map in
      Some (pulses, map'))
    map
  |> Seq.take 1000
  |> Seq.flat_map
       (List.fold ~init:Seq.empty ~f:(fun seq pulse ->
            Seq.cons pulse.intens seq))
  |> Seq.partition (phys_equal Low)
  |> fun (ls, hs) -> Seq.length ls * Seq.length hs

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b

(* big copout tbh, I don't think this works in general, it is very input dependent *)
let part2 map =
  let rx_src =
    Map.to_alist map
    |> List.filter_map ~f:(fun (name, (_, neighs)) ->
           if List.mem neighs "rx" ~equal:String.equal then Some name else None)
    |> List.hd_exn
  in
  (match Map.find_exn map rx_src with
  | Conj deps, _ -> deps
  | _ -> failwith "rx_src is not a conjunction")
  |> Map.keys
  |> List.map ~f:(fun dep ->
         Seq.unfold
           (fun map ->
             let pulses, map' = press_button map in
             Some (pulses, map'))
           map
         |> Seq.take_while (fun pulses ->
                List.exists pulses ~f:(fun pulse ->
                    String.equal pulse.src dep && phys_equal High pulse.intens)
                |> not)
         |> Seq.length |> Int.succ)
  |> List.fold ~init:1 ~f:lcm

let () =
  let map = read_input in
  printf "Part 1: %d\nPart 2: %d\n" (part1 map) (part2 map)
