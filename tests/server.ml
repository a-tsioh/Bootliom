open Eliom_lib 
open Eliom_content

let content =
    let open Bootstrap_shared in
    Html5.F.( [
        div ~a:[a_class ["container"]] [
            Bootstrap_shared.navbar ~classes:["navbar-inverse"]  [
                Bootstrap_shared.Collapse.div "plop" [
                    span ~a:[a_class ["icon-bar"]] [pcdata "a"];
                       span ~a:[a_class ["icon-bar"]] [];

                       ];
                   Bootstrap_shared.dummy_a ~a:[a_class ["brand"]] [
                       pcdata "Menu Title"
                   ];
                   div ~a:[a_class ["nav-collapse";"collapse"]] [
                       ul ~a:[a_class ["nav"]] [
                           li ~a:[a_class ["active"]] [
                               Bootstrap_shared.dummy_a [pcdata "Accueil"]
                               ];
                           li  [
                               Bootstrap_shared.dummy_a  [pcdata "À propos"]
                               ];
                           li  [
                               Bootstrap_shared.dummy_a [pcdata "Contactez-nous"]
                               ]
                           ]
                       ]
            ];
            divcs ["row";"show-grid"] [
               divc "span7" [ 
                   Modal.btn_launch "#modalID" [pcdata "plop"]
               ];
               divc "span5" [
                   pcdata "5"
               ]
            ];
            divcs ["row";"show-grid"] [
               divc "span4" [ 
                   pcdata "4"
               ];
               divc "span4" [
                   pcdata "4"
               ];
               divc "span4" [
                   pcdata "4"
               ];
            ];
            divcs ["row";"show-grid"] [
                divc "span12" [ 
                    pcdata "12"
            ];
            ];
            Modal.create 
                ~title:"Titre"
                ~btn_close:true
                ~footer:[
                    Modal.btn_foot ~default:true [pcdata "plop"];
                    Modal.btn_foot ~dismiss:true [pcdata "close"]
                ]

                "modalID"
                [div [pcdata "corps du modal"]]
            
            ]
        ]);;
