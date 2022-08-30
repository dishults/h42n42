[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ] ] []
let creets_counter_div = div ~a:[ a_class [ "creets-counter" ] ] []
(**)]

[%%client
open Eliom_lib
open Eliom_content
open Js_of_ocaml
open Js_of_ocaml_lwt
open Creet

(* -------------------- Types -------------------- *)

type playground = {
  mutable iter : int;
  mutable global_speed : float ref;
  mutable creets : creet list;
  mutable creets_counter_span : Html_types.span elt;
}

(* -------------------- Utils -------------------- *)

let _update_dom_creets_counter playground =
  let creets_nb = List.length playground.creets in
  let plural = if creets_nb = 1 then ' ' else 's' in
  let new_count = span [ txt (Printf.sprintf "%d creet%c" creets_nb plural) ] in
  let old_count = playground.creets_counter_span in
  Html.Manip.replaceSelf old_count new_count;
  playground.creets_counter_span <- new_count

let _add_creet playground =
  let creet = Creet.create playground.global_speed in
  Html.Manip.appendChild ~%elt creet.elt;
  playground.creets <- creet :: playground.creets;
  _update_dom_creets_counter playground

let _remove_creet playground (creet : creet) =
  Html.Manip.removeSelf creet.elt;
  playground.creets <- List.filter (fun c -> c != creet) playground.creets;
  _update_dom_creets_counter playground

let _move_creet playground creets creet =
  Lwt.async (fun () ->
      let creet_is_alive = Creet.move creets creet in
      if not creet_is_alive then _remove_creet playground creet;
      Lwt.return ())

let _get_available_creets creets = List.filter (fun c -> c.available) creets

let rec _play playground =
  let%lwt () = Lwt_js.sleep 0.001 in
  let healthy, sick =
    List.partition (fun c -> c.state = Healthy) playground.creets
  in
  if List.length healthy = 0 then (
    alert "GAME OVER";
    Lwt.return ())
  else (
    playground.global_speed := !(playground.global_speed) +. 0.0001;
    playground.iter <- playground.iter + 1;
    if playground.iter = 3000 then (
      _add_creet playground;
      playground.iter <- 0);
    let creets =
      {
        healthy = _get_available_creets healthy;
        sick = _get_available_creets sick;
      }
    in
    List.iter (_move_creet playground creets) creets.healthy;
    List.iter (_move_creet playground creets) creets.sick;
    _play playground)

(* -------------------- Main functions -------------------- *)

let play () =
  Random.self_init ();
  let creets_counter_span = span [ txt "0 creets" ] in
  let playground =
    { iter = 0; global_speed = ref 0.; creets = []; creets_counter_span }
  in
  Html.Manip.appendChild ~%creets_counter_div creets_counter_span;
  for _ = 1 to 3 do
    _add_creet playground
  done;
  Lwt.async (fun () -> _play playground)
(**)]
