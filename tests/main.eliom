{shared{
  open Eliom_lib
  open Eliom_content
  open Bootstrap_shared
}}


{server{
  open Server
}}

{client{
  open Client
}}

module test_app =
  Eliom_registration.App (
    struct
      let application_name = "test_Bootliom"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Test_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return (Bootstrap_shared.make_page "titre" Server.content));
