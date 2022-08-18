[%%shared
open Eliom_content
open Eliom_lib
open Html.D
open Js_of_ocaml

let playground_elt = div ~a:[ a_class [ "playground" ] ] []]

[%%client
open CustomTypes

let get_playground () =
  { dom_elt = Eliom_content.Html.To_dom.of_div ~%playground_elt }

let create_creet () =
  let creet_elt = div ~a:[ a_class [ "creet" ] ] [] in
  { dom_elt = Eliom_content.Html.To_dom.of_div creet_elt }

let main () =
  let playground = get_playground () in
  Firebug.console##log_2 (Js.string "playground") playground;

  let creet = create_creet () in
  Firebug.console##log_2 (Js.string "creet") creet;

  Dom.appendChild playground.dom_elt creet.dom_elt]

[%%server
module Game_app = Eliom_registration.App (struct
  let application_name = "game"
  let global_data_path = None
end)

let main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let page =
  body
    [
      div
        ~a:[ a_class [ "gameboard" ] ]
        [
          div ~a:[ a_class [ "river" ] ] [];
          playground_elt;
          (* Hospital is a dashed line at the bottom *)
        ];
    ]

let () =
  Game_app.register ~service:main_service (fun () () ->
      let _ = [%client (main () : unit)] in
      Lwt.return
        (Eliom_tools.D.html ~title:"h42n42" ~css:[ [ "css"; "game.css" ] ] page))]
