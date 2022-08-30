[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt_js_events

(* -------------------- Types -------------------- *)

type creet_state = Healthy | Sick | Berserk | Mean

type creet = {
  (* ----- General ----- *)
  elt : Html_types.div elt;
  dom_elt : Dom_html.divElement Js.t;
  mutable available : bool;
  mutable state : creet_state;
  mutable size : float; (* diameter *)
  mutable speed : float;
  mutable global_speed : float ref;
  mutable iter : int;
  mutable sick_iter : int;
  max_same_direction_iter : int;
  (* ----- Position ----- *)
  mutable top : float;
  mutable top_min : float;
  mutable top_max : float;
  mutable top_step : float;
  mutable left : float;
  mutable left_min : float;
  mutable left_max : float;
  mutable left_step : float;
}

type filtered_creets = { healthy : creet list; sick : creet list }

(* -------------------- Utils -------------------- *)

let _get_bg_color state =
  Js.string
    (match state with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "sienna"
    | Mean -> "tomato")

let _get_creet_defaults () =
  let speed = 1. in
  let size = 50. in
  let top_max = 700. -. size in
  let left_max = 1000. -. size in
  (speed, size, top_max, left_max)

let _get_px number = Js.string (Printf.sprintf "%fpx" number)

let _get_position position step speed global_speed =
  position +. (step *. (speed +. !global_speed))

let _get_random_steps () =
  let step = max 0.25 (Random.float 0.75) in
  let top_step = step in
  let left_step = 1. -. step in
  ( (if Random.bool () = true then top_step else Float.neg top_step),
    if Random.bool () = true then left_step else Float.neg left_step )

let _increase_size creet =
  creet.size <- creet.size +. 0.05;
  creet.top_max <- creet.top_max -. 0.05;
  creet.left_max <- creet.left_max -. 0.05;

  creet.dom_elt##.style##.height := _get_px creet.size;
  creet.dom_elt##.style##.width := _get_px creet.size

let _decrease_size creet =
  creet.size <- creet.size -. 0.0025;
  creet.top_max <- creet.top_max +. 0.0025;
  creet.left_max <- creet.left_max +. 0.0025;

  let size_px = _get_px creet.size in
  creet.dom_elt##.style##.height := size_px;
  creet.dom_elt##.style##.width := size_px

let rec _get_list_min ?(b_i = 1) ?(a_i = 0) a list list_length =
  if b_i < list_length then
    let b = List.nth list b_i in
    if b < a then _get_list_min ~b_i:(b_i + 1) ~a_i:b_i b list list_length
    else _get_list_min ~b_i:(b_i + 1) ~a_i a list list_length
  else (a_i, a)

let _get_distance_between_circles c1 c2 =
  let r1 = c1.size /. 2. in
  let x1 = c1.left +. r1 in
  let y1 = c1.top +. r1 in
  let r2 = c2.size /. 2. in
  let x2 = c2.left +. r2 in
  let y2 = c2.top +. r2 in
  Float.sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.))

let _find_closest_creet c1 creets =
  let distances = List.map (_get_distance_between_circles c1) creets in
  let first_distance = List.hd distances in
  let distances_length = List.length distances in
  let i, _ = _get_list_min first_distance distances distances_length in
  List.nth creets i

let _circles_intersect c1 c2 =
  let r1 = c1.size /. 2. in
  let r2 = c2.size /. 2. in
  let distance = _get_distance_between_circles c1 c2 in
  distance <= r1 +. r2

let _go_after_healthy_creet creet creets =
  let closest_healthy_creet = _find_closest_creet creet creets in
  let top_diff = closest_healthy_creet.top -. creet.top in
  let left_diff = closest_healthy_creet.left -. creet.left in
  let total = Float.abs top_diff +. Float.abs left_diff in
  creet.top_step <- top_diff /. total;
  creet.left_step <- left_diff /. total

let _heal creet =
  (* Reset object *)
  let speed, size, top_max, left_max = _get_creet_defaults () in
  creet.state <- Healthy;
  creet.speed <- speed;
  creet.size <- size;
  creet.top_max <- top_max;
  creet.left_max <- left_max;
  creet.sick_iter <- 0;

  (* Reset DOM element *)
  let size_px = _get_px creet.size in
  creet.dom_elt##.style##.height := size_px;
  creet.dom_elt##.style##.width := size_px;
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state

let _make_sick creet =
  let n = Random.int 100 in
  if n < 10 then creet.state <- Berserk
  else if n >= 10 && n < 20 then creet.state <- Mean
  else creet.state <- Sick;

  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet.speed <- 0.85

let _move creet =
  creet.top <-
    _get_position creet.top creet.top_step creet.speed creet.global_speed;
  creet.left <-
    _get_position creet.left creet.left_step creet.speed creet.global_speed;
  creet.dom_elt##.style##.top := _get_px creet.top;
  creet.dom_elt##.style##.left := _get_px creet.left

let _check_direction creet creets =
  (* Check if creet is on the edge and should turn around *)
  if creet.top <= creet.top_min || creet.top >= creet.top_max then (
    if creet.top <= creet.top_min && creet.state = Healthy then _make_sick creet;
    creet.top_step <- Float.neg creet.top_step;
    _move creet)
  else if creet.left <= creet.left_min || creet.left >= creet.left_max then (
    creet.left_step <- Float.neg creet.left_step;
    (* Those extra moves are needed so that a creet doesn't get stuck on edges *)
    _move creet);

  (* Go after another creet or make a surprise direction change *)
  if creet.state = Mean then (
    creet.iter <- 0;
    if creet.top -. 1. > creet.top_min && List.length creets.healthy > 0 then
      _go_after_healthy_creet creet creets.healthy)
  else if creet.iter = creet.max_same_direction_iter then (
    creet.iter <- 0;
    let top_step, left_step = _get_random_steps () in
    creet.top_step <- top_step;
    creet.left_step <- left_step)

let _event_handler creet event =
  let radius = creet.size /. 2. in
  let left = float_of_int event##.clientX -. radius in
  let top = float_of_int event##.clientY -. radius in
  creet.left <- max creet.left_min (min creet.left_max left);
  creet.top <- max creet.top_min (min creet.top_max top);
  creet.dom_elt##.style##.top := _get_px creet.top;
  creet.dom_elt##.style##.left := _get_px creet.left;
  (* Heal creet if it was taken to the hospital *)
  if creet.state != Healthy && creet.top >= creet.top_max then _heal creet

let _handle_events creet mouse_down _ =
  creet.available <- false;
  creet.dom_elt##.style##.cursor := Js.string "grabbing";
  _event_handler creet mouse_down;
  Lwt.pick
    [
      mousemoves Dom_html.document (fun mouse_move _ ->
          _event_handler creet mouse_move;
          Lwt.return ());
      (let%lwt mouse_up = mouseup Dom_html.document in
       _event_handler creet mouse_up;
       creet.available <- true;
       creet.dom_elt##.style##.cursor := Js.string "grab";
       Lwt.return ());
    ]

(* -------------------- Main functions -------------------- *)

let create global_speed =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let speed, size, top_max, left_max = _get_creet_defaults () in
  let top_step, left_step = _get_random_steps () in
  let creet =
    {
      elt;
      dom_elt = Html.To_dom.of_div elt;
      available = true;
      state = Healthy;
      size;
      speed;
      global_speed;
      iter = 0;
      sick_iter = 0;
      max_same_direction_iter = 2500 + Random.int 1000;
      (* ------------------------------- *)
      top = max 65. (Random.float (top_max -. 15.));
      top_min = 35.;
      top_max;
      top_step;
      left = max 15. (Random.float (left_max -. 15.));
      left_min = 0.;
      left_max;
      left_step;
    }
  in
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet.dom_elt##.style##.height := _get_px creet.size;
  creet.dom_elt##.style##.width := _get_px creet.size;
  Lwt.async (fun () -> mousedowns creet.dom_elt (_handle_events creet));
  creet

let move creets creet =
  creet.iter <- creet.iter + 1;
  _check_direction creet creets;
  _move creet;

  (* Return if creet is alive *)
  match creet.state with
  | Healthy ->
      let _check_intersection sick =
        if _circles_intersect creet sick && Random.int 100 < 2 then
          _make_sick creet
      in
      List.iter _check_intersection creets.sick;
      true
  | Sick ->
      creet.sick_iter <- creet.sick_iter + 1;
      creet.sick_iter < 3000
  | Berserk ->
      if creet.size < 200. then (
        _increase_size creet;
        true)
      else false
  | Mean ->
      if creet.size > 42.5 then (
        _decrease_size creet;
        true)
      else false
(**)]
