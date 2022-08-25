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
  mutable iter : int;
  mutable global_speed : float ref;
  mutable creets : creet list;
}

(* -------------------- Utils -------------------- *)

let _add_creet playground =
  let creet = Creet.create playground.global_speed in
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets

let _remove_creet playground (creet : creet) =
  Html.Manip.removeSelf creet.elt;
  playground.creets <- List.filter (fun c -> c != creet) playground.creets

let _move_creet playground creet =
  Lwt.async (fun () ->
      let creet_is_alive = Creet.move creet in
      if not creet_is_alive then _remove_creet playground creet;
      Lwt.return ())

let _increment_global_speed gs = gs := !gs +. 0.0001

let _is_game_over playground =
  not (List.exists (fun creet -> creet.state = Healthy) playground.creets)

let rec _play playground =
  let%lwt () = Lwt_js.sleep 0.001 in
  if _is_game_over playground then (
    alert "GAME OVER";
    Lwt.return ())
  else (
    _increment_global_speed playground.global_speed;
    playground.iter <- playground.iter + 1;
    if playground.iter = 3000 then (
      _add_creet playground;
      playground.iter <- 0);
    List.iter (_move_creet playground) playground.creets;
    _play playground)

(* -------------------- Main functions -------------------- *)

let get () =
  {
    dom_elt = Html.To_dom.of_div ~%elt;
    iter = 0;
    global_speed = ref 0.;
    creets = [];
  }

let play playground =
  for _ = 1 to 3 do
    _add_creet playground
  done;
  Lwt.async (fun () -> _play playground);
  Lwt.return ()
(**)]
