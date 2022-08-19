[%%client
open Eliom_content
open Html.D
open Js_of_ocaml
open Js_of_ocaml_lwt

let elt = div ~a:[ a_class [ "creet" ] ] []

type coordinates = { mutable top : int; mutable left : int }
type creet = { dom_elt : Dom_html.divElement Js.t; coordinates : coordinates }

let create () =
  { dom_elt = Html.To_dom.of_div elt; coordinates = { top = 0; left = 0 } }

let get_px number () = Js.string (Printf.sprintf "%dpx" number)

let rec move creet () =
  match creet.coordinates.left with
  | 950 -> Lwt.return ()
  | left ->
      let%lwt () = Lwt_js.sleep 0.01 in
      creet.coordinates.left <- left + 1;
      creet.dom_elt##.style##.left := get_px creet.coordinates.left ();

      Firebug.console##log_2 (Js.string "left") creet.coordinates.left;
      move creet ()
(**)]
