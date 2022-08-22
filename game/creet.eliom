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
  mutable top_change : int;
  mutable left : int;
  mutable left_min : int;
  mutable left_max : int;
  mutable left_change : int;
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

let _change_left creet =
  creet.left <- creet.left + creet.left_change;
  creet.dom_elt##.style##.left := _get_px creet.left

let _change_top creet =
  creet.top <- creet.top + creet.top_change;
  creet.dom_elt##.style##.top := _get_px creet.top

let _change_state creet new_state =
  creet.state <- new_state;
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state

(* -------------------- Main functions -------------------- *)

let create () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  let creet =
    {
      (* TODO random direction *)
      dom_elt = Html.To_dom.of_div elt;
      top = 1;
      top_min = -15;
      top_max = 602;
      top_change = 1;
      left = 1;
      left_min = 0;
      left_max = 950;
      left_change = 1;
      state = Healthy;
    }
  in
  creet.dom_elt##.style##.backgroundColor := _get_bg_color creet.state;
  creet

let rec move creet =
  (* TODO increasing speed *)
  let%lwt () = Lwt_js.sleep 0.001 in
  if creet.left = creet.left_min || creet.left = creet.left_max then (
    creet.left_change <- creet.left_change * -1;
    _change_left creet;
    move creet)
  else if creet.top = creet.top_min || creet.top = creet.top_max then (
    (* If touches the toxic river *)
    if creet.top = creet.top_min then _change_state creet Sick;

    creet.top_change <- creet.top_change * -1;
    _change_top creet;
    move creet)
  else (
    _change_top creet;
    _change_left creet;
    move creet)
(**)]
