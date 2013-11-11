open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F



open Bootstrap_shared

(** Faire une page en bonne et due forme *)
let make_page ?(a=[]) ?(css=[]) ?(js=[]) s bodyl =
  Eliom_tools.F.html
    ~title:s
    ~a:a
    ~css:(
      ["bootstrap";"css";"bootstrap.min.css"] ::
        ["bootstrap";"css";"bootstrap-responsive.min.css"] ::
        ["css";"doc.css"]::
        css
    )
    ~js:(
      ["bootstrap";"js";"jquery.js"] ::
        (*            ["bootstrap";"js";"jquery.tools.min.js"] :: *)
        ["bootstrap";"js";"bootstrap.min.js"] :: js)
    (body bodyl)



(** Les menus *)

(* Permet de savoir si un service correspond a l'url courante *)
let same_service_opt ?current s =
  let same_url url =
    make_string_uri ~absolute_path:true ~service:s () = url in
  match current with
  | None ->
    same_url ("/"^(Eliom_request_info.get_current_sub_path_string ()))
  | Some s' -> same_url (make_string_uri ~absolute_path:true ~service:s' ())

(* Une fonction de menu, un poil overkill *)
let menu ?(prefix=[]) ?(postfix=[]) ?(active=["active"]) ?(liclasses=[]) ?(classes=[]) ?id ?service:current l () =
  let rec aux = function
    | [] -> postfix
    | (url, text)::l ->
      (if same_service_opt ?current url
       then (li ~a:[a_class (active@liclasses)] [a url text ()])
       else (li ~a:[a_class liclasses] [a url text ()])) :: (aux l)
  in
  let a_ul = match id with
    | Some id -> [a_id id; a_class classes]
    | None -> [a_class classes]
  in
  ul ~a:a_ul (prefix @ aux l)

(* Permet de wrap une navbar *)
let navbar ?(classes=[]) ?head ?(head_classes=[]) menu =
  let body = match head with
    | Some h -> (divcs ("brand"::head_classes) h) :: menu
    | None -> menu
  in
  divcs ("navbar"::classes) [
    divc "navbar-inner" [
      divc "container" body
    ]]


(** {2 Modal windows} *)



module Modal = struct
  let create ?title ?(btn_close=false) ?(footer=[]) id body =
    let div_close = if btn_close then
        [button ~button_type:`Button
           ~a:[a_class ["close"];
               a_user_data "dismiss" "modal"] [pcdata "×"]]
      else
        [] in
    let div_title = match title with
      | Some s -> [h4 ~a:[a_class ["modal-title"]] [pcdata s]]
      | None -> []
    in
    let div_header = divc "modal-header" (div_close@div_title) in
    let div_footer = divc "modal-footer" footer in
    let div_body = divc "modal-body" body in
    div ~a:[a_class ["modal";"fade"];
            a_id id;
            a_tabindex (-1);
            to_attrib (Xml.string_attrib "role" "dialog")]  [
      divc "modal-dialog" [
        divc "modal-content" [
          div_header ;
          div_body;
          div_footer
        ]
      ]
    ]

  let btn_foot ?(dismiss=false) ?(default=false) content =
    let classes = if default then
        ["btn";"btn-default"]
      else
        ["btn";"btn-primary"]
    in
    if dismiss then
      button ~button_type:`Button
        ~a:[a_class classes;a_user_data "dismiss" "modal"]
        content
    else
      button ~button_type:`Button
        ~a:[a_class classes]
        content



  let btn_launch target content =
    button ~button_type:`Button ~a:[a_class ["btn";"btn-primary";"btn-lg"];
                                    a_user_data "toggle" "modal";
                                    a_user_data "target" target] content
end






