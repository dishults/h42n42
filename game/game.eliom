[%%shared
open Eliom_lib
open Eliom_content
open Html.D]

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

let () =
  Game_app.register ~service:main_service (fun () () ->
      Lwt.return
        (Eliom_tools.D.html ~title:"h42n42"
           ~css:[ [ "css"; "game.css" ] ]
           (page ())))
