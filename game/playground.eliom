[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ] ] []
(**)]

[%%client
open Eliom_content
open Js_of_ocaml
open Creet

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

let add_creet playground (creet : creet) =
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets;
  Firebug.console##log_2 (Js.string "creets_nb") (List.length playground.creets);
  Lwt.return ()
(**)]
