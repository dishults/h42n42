[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt

(* -------------------- Types -------------------- *)

type creet_state = Healthy | Sick | Berserk | Mean

type creet = {
  dom_elt : Dom_html.divElement Js.t;
  mutable top : int;
  mutable top_min : int;
  mutable top_max : int;
  mutable top_direction : int;
  mutable left : int;
  mutable left_min : int;
  mutable left_max : int;
  mutable left_direction : int;
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

let _get_px number = Js.string (Printf.sprintf "%dpx" number)

let _move creet =
  creet.left <- creet.left + creet.left_direction;
  creet.top <- creet.top + creet.top_direction;
  creet.dom_elt##.style##.left := _get_px creet.left;
  creet.dom_elt##.style##.top := _get_px creet.top

let _make_sick creet =
  let n = Random.int 100 in
  if n < 10 then creet.state <- Berserk
  else if n >= 10 && n < 20 then creet.state <- Mean
  else creet.state <- Sick;

  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state

(* -------------------- Main functions -------------------- *)

let create () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let creet =
    {
      dom_elt = Html.To_dom.of_div elt;
      top = max 10 (Random.int 590);
      top_min = -15;
      top_max = 602;
      top_direction = (if Random.bool () = true then 1 else -1);
      left = max 10 (Random.int 940);
      left_min = 0;
      left_max = 950;
      left_direction = (if Random.bool () = true then 1 else -1);
      state = Healthy;
    }
  in
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet

let rec move creet =
  (* TODO increasing speed *)
  let%lwt () = Lwt_js.sleep 0.001 in

  if creet.left = creet.left_min || creet.left = creet.left_max then
    creet.left_direction <- creet.left_direction * -1
  else if creet.top = creet.top_min || creet.top = creet.top_max then (
    if creet.top = creet.top_min && creet.state = Healthy then _make_sick creet;
    creet.top_direction <- creet.top_direction * -1);

  _move creet;
  move creet
(**)]
