[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
(**)]

[%%client
open Js_of_ocaml_lwt

let rec _move (playground : Playground.playground) creet =
  let%lwt () = Lwt_js.sleep 0.001 in
  if playground.game_on then (
    Creet.move creet;
    _move playground creet)
  else Lwt.return ()

let _is_game_over (playground : Playground.playground) =
  let any_healthy (creet : Creet.creet) = creet.state = Healthy in
  List.length playground.creets = 0
  || not (List.exists any_healthy playground.creets)

let rec _check_game_state (playground : Playground.playground) =
  let%lwt () = Lwt_js.sleep 0.01 in
  if _is_game_over playground then (
    playground.game_on <- false;
    alert "GAME OVER";
    Lwt.return ())
  else
    (* TODO playground.global_speed <- playground.global_speed +. 0.001; *)
    _check_game_state playground

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
  Lwt.async (fun () -> _move playground creet);
  Lwt.async (fun () -> _check_game_state playground);
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
    ]

let () =
  Game_app.register ~service:main_service (fun () () ->
      let _ = [%client (main () : unit Lwt.t)] in
      Lwt.return
        (Eliom_tools.D.html ~title:"h42n42" ~css:[ [ "css"; "game.css" ] ] page))
(**)]
