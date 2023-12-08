open Core

type number = { row : int; start_col : int; end_col : int; num : int }
[@@deriving show]

type special_char = { row : int; col : int; ch : char } [@@deriving show]

let get_nums_from_line line row : number list =
  let regex = Str.regexp "\\([0-9]+\\)" in
  let rec get_nums start_pos =
    try
      let start_col = Str.search_forward regex line start_pos in
      let num_str = Str.matched_group 1 line in
      let end_col = start_col + String.length num_str - 1 in
      { row; start_col; end_col; num = Int.of_string num_str }
      :: get_nums (end_col + 1)
    with _ -> []
  in
  get_nums 0 |> List.rev

let get_special_chars_from_line line row : special_char list =
  String.to_list line
  |> List.foldi ~init:[] ~f:(fun col acc ch ->
         if Char.(equal ch '.' || is_digit ch) then acc
         else { row; col; ch } :: acc)

let num_touches_sp_char (num : number) sp_ch =
  let north_or_south =
    (equal (sp_ch.row - 1) num.row || equal (sp_ch.row + 1) num.row)
    && sp_ch.col >= num.start_col - 1
    && sp_ch.col <= num.end_col + 1
  in
  let east_or_west =
    (equal (sp_ch.col + 1) num.start_col || equal (sp_ch.col - 1) num.end_col)
    && equal sp_ch.row num.row
  in
  north_or_south || east_or_west

let is_gear (nums : number list) sp_char : int option =
  if not (Char.equal sp_char.ch '*') then None
  else
    let touching_nums =
      List.filter nums ~f:(fun num -> num_touches_sp_char num sp_char)
    in
    match touching_nums with
    | [ num1; num2 ] -> Some (num1.num * num2.num)
    | _ -> None

let is_valid_num sp_chars (num : number) : bool =
  List.exists sp_chars ~f:(num_touches_sp_char num)

let () =
  let lines = In_channel.read_lines "input.in" in
  let nums =
    List.foldi lines ~init:[] ~f:(fun row acc l ->
        List.append (get_nums_from_line l row) acc)
  in
  let sp_chars =
    List.foldi lines ~init:[] ~f:(fun row acc l ->
        List.append (get_special_chars_from_line l row) acc)
  in
  let sum1 =
    List.filter nums ~f:(is_valid_num sp_chars)
    |> List.sum (module Int) ~f:(fun num -> num.num)
  in
  let sum2 =
    List.filter_map sp_chars ~f:(is_gear nums) |> List.sum (module Int) ~f:Fn.id
  in
  printf "Part 1: %d\nPart 2: %d\n" sum1 sum2
