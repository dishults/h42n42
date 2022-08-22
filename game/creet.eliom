[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt

(* -------------------- Types -------------------- *)

type creet_state = Healthy | Sick | Berserk | Mean

type creet = {
  dom_elt : Dom_html.divElement Js.t;
  mutable speed : float; (* TODO global speed passed from playground *)
  mutable top : float;
  mutable top_min : int;
  mutable top_max : int;
  mutable top_step : int;
  mutable left : float;
  mutable left_min : int;
  mutable left_max : int;
  mutable left_step : int;
  mutable state : creet_state;
}

(* -------------------- Utils -------------------- *)

let _get_bg_color state =
  Js.string
    (match state with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "darkcyan"
    | Mean -> "tomato")

let _get_px number = Js.string (Printf.sprintf "%fpx" number)
let _get_step position step speed = position +. (float_of_int step *. speed)

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

let create () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let creet =
    {
      dom_elt = Html.To_dom.of_div elt;
      speed = 1.;
      top = max 10. (Random.float 590.);
      top_min = -15;
      top_max = 602;
      top_step = (if Random.bool () = true then 1 else -1);
      left = max 10. (Random.float 940.);
      left_min = 0;
      left_max = 950;
      left_step = (if Random.bool () = true then 1 else -1);
      state = Healthy;
    }
  in
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet

let rec move creet =
  (* TODO increasing speed *)
  let%lwt () = Lwt_js.sleep 0.001 in

  if List.mem (int_of_float creet.top) [ creet.top_min; creet.top_max ] then (
    if int_of_float creet.top = creet.top_min && creet.state = Healthy then
      _make_sick creet;
    creet.top_step <- creet.top_step * -1;
    _move creet)
  else if List.mem (int_of_float creet.left) [ creet.left_min; creet.left_max ]
  then (
    creet.left_step <- creet.left_step * -1;
    _move creet);

  (* The above extra moves are needed so that a slow sick creet doesn't get stuck on the edge *)
  _move creet;
  move creet
(**)]
