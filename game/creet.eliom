[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt

(* -------------------- Types -------------------- *)

type creet_state = Healthy | Sick | Berserk | Mean

type creet = {
  (* ----- General ----- *)
  elt : Html_types.div elt;
  dom_elt : Dom_html.divElement Js.t;
  mutable state : creet_state;
  mutable size : float;
  mutable speed : float; (* TODO global speed passed from playground *)
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

(* -------------------- Utils -------------------- *)

let _get_bg_color state =
  Js.string
    (match state with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "sienna"
    | Mean -> "tomato")

let _get_px number = Js.string (Printf.sprintf "%fpx" number)
let _get_step position step speed = position +. (step *. speed)

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

  creet.dom_elt##.style##.height := _get_px creet.size;
  creet.dom_elt##.style##.width := _get_px creet.size

let _change_direction creet =
  if creet.state != Healthy then creet.sick_iter <- creet.sick_iter + 1;
  if creet.state = Mean then
    creet.iter <- 0 (* TODO go after a healthy creet if exist *)
  else if creet.iter = creet.max_same_direction_iter then (
    creet.iter <- 0;
    let top_step, left_step = _get_random_steps () in
    creet.top_step <- top_step;
    creet.left_step <- left_step)
  else creet.iter <- creet.iter + 1

let _make_sick creet =
  let n = Random.int 100 in
  if n < 10 then creet.state <- Berserk
  else if n >= 10 && n < 20 then creet.state <- Mean
  else creet.state <- Sick;

  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet.speed <- 0.85

let _move creet =
  creet.top <- _get_step creet.top creet.top_step creet.speed;
  creet.left <- _get_step creet.left creet.left_step creet.speed;
  creet.dom_elt##.style##.top := _get_px creet.top;
  creet.dom_elt##.style##.left := _get_px creet.left

(* -------------------- Main functions -------------------- *)

let create () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let size = 50. in
  let top_max = 700. -. size in
  let left_max = 1000. -. size in
  let top_step, left_step = _get_random_steps () in
  let creet =
    {
      elt;
      dom_elt = Html.To_dom.of_div elt;
      state = Healthy;
      size;
      speed = 1.;
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
  creet

let move creet =
  if creet.top <= creet.top_min || creet.top >= creet.top_max then (
    if creet.top <= creet.top_min && creet.state = Healthy then _make_sick creet;
    creet.top_step <- Float.neg creet.top_step;
    _move creet)
  else if creet.left <= creet.left_min || creet.left >= creet.left_max then (
    creet.left_step <- Float.neg creet.left_step;
    _move creet);

  _change_direction creet;
  (* The above extra moves are needed so that a slow sick creet doesn't get stuck on the edge *)
  _move creet;

  (* Return if creet is alive *)
  match creet.state with
  | Healthy -> true
  | Sick -> creet.sick_iter < 3000
  | Berserk when creet.size < 200. ->
      _increase_size creet;
      true
  | Mean when creet.size > 42.5 ->
      _decrease_size creet;
      true
  | _ -> false
(**)]
