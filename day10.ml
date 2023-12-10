open Core

let read_input =
  In_channel.read_lines "input.in"
  |> List.map ~f:String.to_array
  |> Array.of_list

let size pipes = (Array.length pipes, pipes.(0) |> Array.length)
let size_dot pipes = (Array.length pipes + 1, (pipes.(0) |> Array.length) + 1)

let ( <-> ) s e =
  if s > e then failwith "start greater than end"
  else List.init (e - s) ~f:(fun n -> n + s)

type dir = N | S | W | E [@@deriving eq]

let get_dir = function
  | '|' -> [ N; S ]
  | '-' -> [ W; E ]
  | 'L' -> [ N; E ]
  | 'J' -> [ N; W ]
  | '7' -> [ S; W ]
  | 'F' -> [ S; E ]
  | 'S' -> [ N; S; W; E ]
  | '.' -> []
  | _ -> failwith "wrong pipe code"

let pos_to_num pipes (y, x) =
  let _, n = size pipes in
  x + (y * n)

let pos_to_num_dot pipes (y, x) =
  let _, n = size_dot pipes in
  x + (y * n)

let num_to_pos_dot pipes num_dot =
  let _, n = size_dot pipes in
  let x = num_dot mod n in
  (num_dot / n, x)

let neighbours pipes (y, x) =
  let m, n = size pipes in
  [ (0, 1, E); (0, -1, W); (1, 0, S); (-1, 0, N) ]
  |> List.filter_map ~f:(fun (dy, dx, dir) ->
         let x' = x + dx in
         let y' = y + dy in
         if x' >= 0 && x' < n && y' >= 0 && y' < m then
           Some (pipes.(y').(x'), y', x', dir)
         else None)

let neighbours_dot pipes (y, x) =
  let m, n = size_dot pipes in
  [ (0, 1, E); (0, -1, W); (1, 0, S); (-1, 0, N) ]
  |> List.filter_map ~f:(fun (dy, dx, dir) ->
         let x' = x + dx in
         let y' = y + dy in
         if x' >= 0 && x' < n && y' >= 0 && y' < m then Some (y', x', dir)
         else None)

let valid_neighbours pipes (y, x) =
  let own_dir = get_dir pipes.(y).(x) in
  neighbours pipes (y, x)
  |> List.filter_map ~f:(fun (pipe, y, x, dir) ->
         if not (List.mem own_dir dir ~equal:equal_dir) then None
         else
           let neigh_dirs = get_dir pipe in
           let some_if_contains dire =
             if List.mem neigh_dirs dire ~equal:equal_dir then Some (y, x)
             else None
           in
           match dir with
           | N -> some_if_contains S
           | S -> some_if_contains N
           | W -> some_if_contains E
           | E -> some_if_contains W)

let filter_valid_neighbours_dot pipes main_pipe (yd, xd)
    (neighbours : (int * int * dir) list) =
  let contains_dir (yc, xc) dir =
    List.mem (get_dir pipes.(yc).(xc)) dir ~equal:equal_dir
  in
  let is_in_main (yc, xc) = Hashtbl.mem main_pipe (pos_to_num pipes (yc, xc)) in
  neighbours
  |> List.filter ~f:(fun (_, _, dir) ->
         (match dir with
         | E ->
             is_in_main (yd, xd)
             && is_in_main (yd - 1, xd)
             && contains_dir (yd, xd) N
             && contains_dir (yd - 1, xd) S
         | W ->
             is_in_main (yd, xd - 1)
             && is_in_main (yd - 1, xd - 1)
             && contains_dir (yd, xd - 1) N
             && contains_dir (yd - 1, xd - 1) S
         | S ->
             is_in_main (yd, xd)
             && is_in_main (yd, xd - 1)
             && contains_dir (yd, xd) W
             && contains_dir (yd, xd - 1) E
         | N ->
             is_in_main (yd - 1, xd)
             && is_in_main (yd - 1, xd - 1)
             && contains_dir (yd - 1, xd) W
             && contains_dir (yd - 1, xd - 1) E)
         |> not)

let get_start pipes =
  let y = ref 0 in
  let x = ref 0 in
  let _ =
    pipes
    |> Array.find_exn ~f:(fun pipe_row ->
           x := 0;
           y := !y + 1;
           pipe_row
           |> Array.find ~f:(fun pipe ->
                  x := !x + 1;
                  Char.equal pipe 'S')
           |> Option.is_some)
  in
  (!y - 1, !x - 1)

let bfs pipes (s_y, s_x) =
  let q = Queue.create () in
  let visited = Hashtbl.create (module Int) in
  Queue.enqueue q (s_y, s_x, 0);
  while not (Queue.is_empty q) do
    let t_y, t_x, t_dist = Queue.dequeue_exn q in
    let t_num = pos_to_num pipes (t_y, t_x) in
    if Hashtbl.mem visited t_num then ()
    else (
      Hashtbl.add_exn visited ~key:t_num ~data:t_dist;
      valid_neighbours pipes (t_y, t_x)
      |> List.iter ~f:(fun (n_y, n_x) ->
             let n_num = pos_to_num pipes (n_y, n_x) in
             if Hashtbl.mem visited n_num then ()
             else Queue.enqueue q (n_y, n_x, t_dist + 1)))
  done;
  visited

let bfs_2 main_pipe pipes =
  let outside_tbl = Hashtbl.create (module Int) in
  let add_to_outside (y, x) =
    let key = pos_to_num_dot pipes (y, x) in
    Hashtbl.add_exn outside_tbl ~key ~data:()
  in
  let md, nd = size_dot pipes in

  let q = Queue.create () in
  0 <-> md
  |> List.iter ~f:(fun yd ->
         add_to_outside (yd, 0);
         add_to_outside (yd, nd - 1));
  List.init (nd - 2) ~f:Int.succ
  |> List.iter ~f:(fun xd ->
         add_to_outside (0, xd);
         add_to_outside (md - 1, xd));
  Hashtbl.to_alist outside_tbl
  |> List.map ~f:(fun (k, _) -> num_to_pos_dot pipes k)
  |> Queue.enqueue_all q;

  while not (Queue.is_empty q) do
    let tyd, txd = Queue.dequeue_exn q in
    neighbours_dot pipes (tyd, txd)
    |> List.filter ~f:(fun (y_d, x_d, _) ->
           let num_d = pos_to_num_dot pipes (y_d, x_d) in
           Hashtbl.mem outside_tbl num_d |> not)
    |> filter_valid_neighbours_dot pipes main_pipe (tyd, txd)
    |> List.iter ~f:(fun (nyd, nxd, _) ->
           let numd = pos_to_num_dot pipes (nyd, nxd) in
           Hashtbl.add_exn outside_tbl ~key:numd ~data:();
           Queue.enqueue q (nyd, nxd))
  done;
  outside_tbl

let get_inside_pipes pipes main_pipe outside_tbl =
  let m, n = size pipes in
  List.cartesian_product (0 <-> m) (0 <-> n)
  |> List.filter ~f:(fun (y, x) ->
         let is_not_outside dot =
           Hashtbl.mem outside_tbl (pos_to_num_dot pipes dot) |> not
         in
         let is_not_main =
           Hashtbl.mem main_pipe (pos_to_num pipes (y, x)) |> not
         in
         is_not_main
         && [ (0, 0); (0, 1); (1, 0); (1, 1) ]
            |> List.for_all ~f:(fun (dy, dx) -> is_not_outside (y + dy, x + dx)))

let () =
  let pipes = read_input in
  let s_y, s_x = get_start pipes in
  let main_pipe = bfs pipes (s_y, s_x) in
  let dist =
    Hashtbl.fold main_pipe ~init:0 ~f:(fun ~key:_ ~data acc -> Int.max data acc)
  in
  let outside_tbl = bfs_2 main_pipe pipes in
  let inside_cnt =
    get_inside_pipes pipes main_pipe outside_tbl |> List.length
  in
  printf "Part 1: %d\nPart 2: %d\n" dist inside_cnt
