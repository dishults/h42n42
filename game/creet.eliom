[%%client
open Eliom_content
open Html.D
open Js_of_ocaml

let elt = div ~a:[ a_class [ "creet" ] ] []

type creet = { dom_elt : Dom_html.divElement Js.t }

let create () = { dom_elt = Html.To_dom.of_div elt }]
