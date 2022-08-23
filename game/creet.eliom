[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt

(* -------------------- Types -------------------- *)

type creet_state = Healthy | Sick | Berserk | Mean

type creet = {
  (* ----- General ----- *)
  dom_elt : Dom_html.divElement Js.t;
  mutable state : creet_state;
  mutable size : float;
  mutable speed : float; (* TODO global speed passed from playground *)
  mutable counter : int;
  max_counter : int;
  (* ----- Position ----- *)
  mutable top : float;
  mutable top_min : int;
  mutable top_max : int;
  mutable top_step : float;
  mutable left : float;
  mutable left_min : int;
  mutable left_max : int;
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

let _increase_size creet =
  let before = int_of_float creet.size in
  creet.size <- creet.size +. 0.01;
  let after = int_of_float creet.size in
  let difference = after - before in
  creet.top_max <- creet.top_max - difference;
  creet.left_max <- creet.left_max - difference;

  creet.dom_elt##.style##.height := _get_px creet.size;
  creet.dom_elt##.style##.width := _get_px creet.size

let _decrease_size creet =
  let before = int_of_float creet.size in
  creet.size <- creet.size -. 0.01;
  let after = int_of_float creet.size in
  let difference = before - after in
  creet.top_max <- creet.top_max + difference;
  creet.left_max <- creet.left_max + difference;

  creet.dom_elt##.style##.height := _get_px creet.size;
  creet.dom_elt##.style##.width := _get_px creet.size

let _change_direction creet =
  if creet.state = Mean then
    creet.counter <- 0 (* TODO go after a healthy creet if exist *)
  else if creet.counter = creet.max_counter then (
    creet.counter <- 0;
    let step = Random.float 1. in
    creet.top_step <- max 0.25 step;
    creet.left_step <- max 0.25 (1. -. step))
  else creet.counter <- creet.counter + 1

let _move creet =
  creet.top <- _get_step creet.top creet.top_step creet.speed;
  creet.left <- _get_step creet.left creet.left_step creet.speed;
  creet.dom_elt##.style##.top := _get_px creet.top;
  creet.dom_elt##.style##.left := _get_px creet.left

let _make_sick creet =
  let n = Random.int 100 in
  if n < 10 then creet.state <- Berserk
  else if n >= 10 && n < 20 then creet.state <- Mean
  else creet.state <- Sick;

  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet.speed <- creet.speed *. 0.85

(* -------------------- Main functions -------------------- *)

let create size =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let step = Random.float 1. in
  let creet =
    {
      dom_elt = Html.To_dom.of_div elt;
      state = Healthy;
      size;
      speed = 1.;
      counter = 0;
      max_counter = 2500 + Random.int 1000;
      (* ------------------------------- *)
      top = max 10. (Random.float 590.);
      top_min = -15;
      top_max = 602;
      top_step = max 0.25 step;
      left = max 10. (Random.float 940.);
      left_min = 0;
      left_max = 950;
      left_step = max 0.25 (1. -. step);
    }
  in
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet.dom_elt##.style##.height := _get_px creet.size;
  creet.dom_elt##.style##.width := _get_px creet.size;
  creet

let move creet =
  if List.mem (int_of_float creet.top) [ creet.top_min; creet.top_max ] then (
    if int_of_float creet.top = creet.top_min && creet.state = Healthy then
      _make_sick creet;
    creet.top_step <- Float.neg creet.top_step;
    _move creet)
  else if List.mem (int_of_float creet.left) [ creet.left_min; creet.left_max ]
  then (
    creet.left_step <- Float.neg creet.left_step;
    _move creet);

  (match creet.state with
  | Berserk when creet.size < 200. -> _increase_size creet
  | Mean when creet.size > 42.5 -> _decrease_size creet
  | _ -> ());

  _change_direction creet;
  (* The above extra moves are needed so that a slow sick creet doesn't get stuck on the edge *)
  _move creet
(**)]
