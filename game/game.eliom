[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
(**)]

[%%client
let main () =
  Random.self_init ();
  let playground = Playground.get () in
  Firebug.console##log_2 (Js.string "playground") playground;

  let creet =
    Creet.create playground.creet_size playground.creet_top_max
      playground.creet_left_max
  in
  Firebug.console##log_2 (Js.string "creet") creet;

  Lwt.async (fun () -> Playground.add_creet playground creet);
  Lwt.async (fun () -> Playground.play playground);
  Lwt.return ()
(**)]

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
          Playground.elt;
          (* Hospital is a dashed line at the bottom *)
        ];
      (* TODO add creets counter *)
    ]

let () =
  Game_app.register ~service:main_service (fun () () ->
      let _ = [%client (main () : unit Lwt.t)] in
      Lwt.return
        (Eliom_tools.D.html ~title:"h42n42" ~css:[ [ "css"; "game.css" ] ] page))
(**)]
