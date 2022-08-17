[%%shared
open Eliom_content
open Eliom_lib
open Html.D
open Js_of_ocaml]

module Game_app = Eliom_registration.App (struct
  let application_name = "game"
  let global_data_path = None
end)

let main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let playground_elt = div ~a:[ a_class [ "playground" ] ] []

let page () =
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

[%%client
type creet = { elt : Html_types.div elt; dom_elt : Dom_html.divElement Js.t }

let create_creet () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  { elt; dom_elt = Eliom_content.Html.(To_dom.of_div elt) }

let init_client () =
  let playground = Eliom_content.Html.To_dom.of_div ~%playground_elt in
  Firebug.console##log_2 (Js.string "playground") playground;

  let creet = create_creet () in
  Firebug.console##log_2 (Js.string "creet") creet;

  Dom.appendChild playground creet.dom_elt]

let () =
  Game_app.register ~service:main_service (fun () () ->
      let _ = [%client (init_client () : unit)] in
      Lwt.return
        (Eliom_tools.D.html ~title:"h42n42"
           ~css:[ [ "css"; "game.css" ] ]
           (page ())))
