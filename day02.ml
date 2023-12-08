open Core

type round = { red : int; green : int; blue : int } [@@deriving show]
type game = { id : int; rounds : round list } [@@deriving show]

let ( << ) f g x = f (g x)

let line_to_game l =
  let _ = Str.search_forward (Str.regexp {|Game \([0-9]+\)|}) l 0 in
  let id = Str.matched_group 1 l |> Int.of_string in
  let rounds_str = String.split ~on:':' l |> List.last_exn in
  let rounds = String.split ~on:';' rounds_str in
  let rounds =
    List.map rounds ~f:(fun rnds ->
        String.split ~on:',' rnds
        |> List.fold ~init:{ red = 0; green = 0; blue = 0 } ~f:(fun acc rnd ->
               let _ =
                 Str.search_forward (Str.regexp {|\([0-9]+\) \([a-z]+\)|}) rnd 0
               in
               let number = Str.matched_group 1 rnd |> Int.of_string in
               let color = Str.matched_group 2 rnd in
               match color with
               | "red" -> { acc with red = number }
               | "green" -> { acc with green = number }
               | "blue" -> { acc with blue = number }
               | _ -> acc))
  in
  { id; rounds }

let round_is_possible round =
  round.red <= 12 && round.green <= 13 && round.blue <= 14

let game_is_possible game = List.for_all game.rounds ~f:round_is_possible

let min_cube_set game =
  List.fold game.rounds ~init:{ red = 0; green = 0; blue = 0 }
    ~f:(fun acc round ->
      {
        red = Int.max acc.red round.red;
        green = Int.max acc.green round.green;
        blue = Int.max acc.blue round.blue;
      })

let power_of_min_set game =
  let { red; green; blue } = min_cube_set game in
  red * green * blue

let () =
  let lines = In_channel.read_lines "input.in" in
  let games = List.map lines ~f:line_to_game in
  let sum1 =
    List.filter games ~f:game_is_possible
    |> List.fold ~init:0 ~f:(fun acc game -> acc + game.id)
  in
  let sum2 = List.map games ~f:power_of_min_set |> List.fold ~init:0 ~f:( + ) in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
