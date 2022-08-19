[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ] ] []]

[%%client
open Eliom_content
open Js_of_ocaml

type playground = { dom_elt : Dom_html.divElement Js.t }

let get () = { dom_elt = Html.To_dom.of_div ~%elt }]
