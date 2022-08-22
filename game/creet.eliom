[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt

let elt = div ~a:[ a_class [ "creet" ] ] []

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
}

let create () =
  {
    (* TODO random direction *)
    dom_elt = Html.To_dom.of_div elt;
    top = 1;
    top_min = 0;
    top_max = 602;
    top_change = 1;
    left = 1;
    left_min = 0;
    left_max = 950;
    left_change = 1;
  }

let get_px number () = Js.string (Printf.sprintf "%dpx" number)

let change_left creet () =
  creet.left <- creet.left + creet.left_change;
  creet.dom_elt##.style##.left := get_px creet.left ()

let change_top creet () =
  creet.top <- creet.top + creet.top_change;
  creet.dom_elt##.style##.top := get_px creet.top ()

let rec move creet () =
  (* TODO increasing speed *)
  let%lwt () = Lwt_js.sleep 0.001 in
  if creet.left = creet.left_min || creet.left = creet.left_max then (
    creet.left_change <- creet.left_change * -1;
    change_left creet ();
    move creet ())
  else if creet.top = creet.top_min || creet.top = creet.top_max then (
    creet.top_change <- creet.top_change * -1;
    change_top creet ();
    move creet ())
  else (
    change_top creet ();
    change_left creet ();
    move creet ())
(**)]
