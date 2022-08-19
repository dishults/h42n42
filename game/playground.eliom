[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ] ] []
(**)]

[%%client
open Eliom_content
open Js_of_ocaml

type playground = {
  dom_elt : Dom_html.divElement Js.t;
  mutable creets : Creet.creet list;
}

let get () = { dom_elt = Html.To_dom.of_div ~%elt; creets = [] }

let add_creet playground (creet : Creet.creet) =
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets;
  Firebug.console##log_2 (Js.string "creets_nb") (List.length playground.creets);
  Lwt.return ()
(**)]
