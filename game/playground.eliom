[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ] ] []
(**)]

[%%client
open Eliom_lib
open Eliom_content
open Js_of_ocaml
open Js_of_ocaml_lwt
open Creet

(* -------------------- Types -------------------- *)

type playground = {
  dom_elt : Dom_html.divElement Js.t;
  creet_size : float;
  creet_size_min : float;
  creet_size_max : float;
  creet_top_max : float;
  creet_left_max : float;
  mutable speed : float;
  mutable game_on : bool;
  mutable creets : creet list;
}

(* -------------------- Utils -------------------- *)

let _add_creet playground (creet : creet) =
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets

let _remove_creet playground (creet : creet) =
  Dom.removeChild playground.dom_elt creet.dom_elt;
  playground.creets <- List.filter (fun c -> c != creet) playground.creets

let _move_creet playground (creet : creet) =
  Lwt.async (fun () ->
      let creet_is_alive = Creet.move creet in
      if not creet_is_alive then _remove_creet playground creet;
      Lwt.return ())

let _is_game_over playground =
  List.length playground.creets = 0
  || not (List.exists (fun creet -> creet.state = Healthy) playground.creets)

let rec _play playground =
  let%lwt () = Lwt_js.sleep 0.001 in
  if _is_game_over playground then (
    alert "GAME OVER";
    Lwt.return ())
  else (
    (* TODO playground.global_speed <- playground.global_speed +. 0.0001; *)
    List.iter (_move_creet playground) playground.creets;
    _play playground)

(* -------------------- Main functions -------------------- *)

let get () =
  let creet_size = 50. in
  {
    dom_elt = Html.To_dom.of_div ~%elt;
    creet_size;
    creet_size_min = creet_size *. 0.85;
    creet_size_max = creet_size *. 4.;
    creet_top_max = 652. -. creet_size;
    creet_left_max = 1000. -. creet_size;
    game_on = true;
    speed = 1.;
    creets = [];
  }

let play playground =
  let creet =
    Creet.create playground.creet_size playground.creet_top_max
      playground.creet_left_max
  in
  _add_creet playground creet;
  Lwt.async (fun () -> _play playground);
  Lwt.return ()
(**)]
