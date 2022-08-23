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
  mutable speed : float;
  mutable creets : creet list;
}

let get () =
  let creet_size = 50. in
  {
    dom_elt = Html.To_dom.of_div ~%elt;
    creet_size;
    creet_size_min = creet_size *. 0.85;
    creet_size_max = creet_size *. 4.;
    speed = 1.;
    creets = [];
  }

let add_creet playground (creet : creet) =
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets;
  Firebug.console##log_2 (Js.string "creets_nb") (List.length playground.creets);
  Lwt.return ()
(**)]
