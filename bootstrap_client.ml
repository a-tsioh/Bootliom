open Bootstrap_shared;;

module ToogleClass (Class : CLASS) = struct

        let up node =
        node##classList##add(Js.string Class.v)
        let down node =
        node##classList##remove(Js.string Class.v)
        let toogle node =
        node##classList##toogle(Js.string Class.v)

        let make_onoff event_on event_off element target =
        let on = if Class.revert then down else up in
        let off = if Class.revert then up else down in
        Lwt.async (fun () ->
                event_on
                element
                (fun _ _ ->
                 on target ;
                 event_off element >|= (fun _ -> off target)))

        let make_toogle event_toogle element target =
        Lwt.async (fun () ->
                event_toogle
                element
                (fun _ _ -> toogle target))

        let onhover element target =
        make_onoff
        Lwt_js_events.mouseovers Lwt_js_events.mouseout
        element target

        let onclic element target =
        make_toogle
        Lwt_js_events.clicks
        element target
        end
    module Typeahead = struct

        open Js
        module U = Js.Unsafe

        let apply
        (* The method used to determine if a query matches an item. *)
        ?(matcher : (js_string t -> bool t) option)
        (* Method used to sort autocomplete results. *)
        ?(sorter : (js_string t js_array t -> js_string t js_array t) option)
        (* Method used to highlight autocomplete results. *)
        ?(highlighter : (js_string t -> #Dom_html.element t) option)
        (* The method used to return selected item. *)
        ?(updater : (js_string t -> js_string t) option)
        (* The data source to query against. *)
        ?(source : js_string t js_array t option)
        (* The max number of items to display in the dropdown. default : 8 *)
        ?(items : int option)
        (* The dropdown menu. default : <ul class="typeahead dropdown-menu"></ul> *)
        ?(menu : #Dom_html.element option)
        (* A dropdown item. default : <li><a href="#"></a></li> *)
        ?(item : #Dom_html.element option)
        (* The minimum character length needed before triggering autocomplete suggestions. default : 1 *)
        ?(minLength : int option)
        (* The input object *)
        (i : #Dom_html.inputElement t)
        =
        let opt_inject x = opt_map U.inject x in
        let user_data =
        [ "matcher", opt_inject matcher ;
    "sorter", opt_inject sorter ;
    "highlighter", opt_inject highlighter ;
    "updater", opt_inject updater ;
    "source", opt_inject source ;
    "items", opt_inject items ;
    "menu", opt_inject menu ;
    "item", opt_inject item ;
    "minLength", opt_inject minLength ;
    ] in
        let rec make_object obj = function
        | [] -> obj
        | (_, None)::l -> make_object obj l
        | (s, Some v) :: l -> U.set obj s v ; make_object obj l
        in
        let obj = make_object (U.obj [| |]) user_data in
        let data = U.fun_call (U.variable "jQuery") [|U.inject i|] in
        ignore (U.meth_call data "typeahead" [| U.inject obj|] )


        end

  module Popover = struct

        open Js
        open Popover_html
        module U = Js.Unsafe

        let js_position =
        let right = string "right" in
        let left = string "left" in
        let top = string "top" in
        let bottom = string "bottom" in
        function
        | Right -> right
        | Left -> left
        | Top -> top
        | Bottom -> bottom


        let js_trigger =
        let click = string "click" in
        let hover = string "hover" in
        let focus = string "focus" in
        let manual = string "manual" in
        function
        | Click -> click
        | Hover -> hover
        | Focus -> focus
        | Manual -> manual

        let apply
        ?(html : bool t option)
        ?(animation : bool t option)
        ?placement
        ?(selector : js_string t option)
        ?trigger
        ?(title : #Dom_html.element t option)
        ?(content : #Dom_html.element t option)
        ?(delay : float t option)
        ?(container : js_string t option)
        (e : #Dom_html.element t)
        =
        let opt_inject x = opt_map U.inject x in
        let placement = opt_map (fun x -> U.inject (js_position x)) placement in
        let trigger =  opt_map (fun x -> U.inject (js_trigger x)) trigger in
        let title = opt_map (fun x -> U.inject x##innerHTML) title in
        let content = opt_map (fun x -> U.inject x##innerHTML) content in
        let user_data =
        [ "html", opt_inject html ;
    "animation", opt_inject animation ;
    "placement", placement ;
    "selector", opt_inject selector ;
    "trigger", trigger ;
    "title", title ;
    "content", content ;
    "delay", opt_inject delay ;
    "container", opt_inject container ;
    ] in
        let rec make_object obj = function
        | [] -> obj
        | (_, None)::l -> make_object obj l
        | (s, Some v) :: l -> U.set obj s v ; make_object obj l
        in
        let obj = make_object (U.obj [| |]) user_data in
        let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        ignore (U.meth_call data "popover" [| U.inject obj|] )

        let show (e: #Dom_html.element t) =
        let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        ignore (U.meth_call data "popover" [| U.inject (string "show")|])

        let hide (e: #Dom_html.element t) =
        let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        ignore (U.meth_call data "popover" [| U.inject (string "hide")|])

        let toogle (e: #Dom_html.element t) =
        let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        ignore (U.meth_call data "popover" [| U.inject (string "toogle")|])

        let destroy (e: #Dom_html.element t) =
        let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        ignore (U.meth_call data "popover" [| U.inject (string "destroy")|])

        end
    module Tooltip = struct

        open Js
        module U = Js.Unsafe

        open Tooltip_html

        let js_pos_h =
        let right = string "right" in
        let left = string "left" in
        let center = string "center" in
        function
        | `Right -> right
        | `Left -> left
        | `Center -> center

        let js_pos_v =
        let top = string "top" in
        let bottom = string "bottom" in
        let center = string "center" in
        function
        | `Bottom -> bottom
        | `Top -> top
        | `Center -> center

        let js_pos (v,h) = (js_pos_v v)##concat_2(string " ",js_pos_h h)

        (* Argument not binded yet :
         events, take an object
         layout, take html as string
         *)


        let apply
        ?(cancelDefault : bool t option)
        ?(effect : js_string t option)
        ?(delay : int option)
        ?(offset : (int * int) option)
        ?(opacity : int option)
        ?(position : (pos_v * pos_h) option)
        ?(predelay : int option)
        ?(relative : bool t option)
        ?(tip : js_string t option)
        ?(tipClass : js_string t option)
        e
        =
        let e = string e in
        let opt_inject x = opt_map U.inject x in
        let position = opt_map (fun x -> U.inject (js_pos x)) position in
        let offset = opt_map (fun (x,y) -> U.inject (array [| x ; y |])) offset in
        let user_data =
        [ "cancelDefault", opt_inject cancelDefault ;
    "effect", opt_inject effect ;
    "delay", opt_inject delay ;
    "offset", offset ;
    "opacity", opt_inject opacity ;
    "position", position ;
    "predelay", opt_inject predelay ;
    "relative", opt_inject relative;
    "tip", opt_inject tip;
    "tipClass", opt_inject tipClass;
    ] in
        let rec make_object obj = function
        | [] -> obj
        | (_, None)::l -> make_object obj l
        | (s, Some v) :: l -> U.set obj s v ; make_object obj l
        in
        let obj = make_object (U.obj [| |]) user_data in
        let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        ignore (U.meth_call data "tooltip" [| U.inject obj|] )

        let show (e: #Dom_html.element t) =
        Eliom_lib.debug "show !" ;
    let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        let api = U.meth_call data "data" [| U.inject (string "tooltip") |] in
        ignore (U.meth_call api "show" [| |])

        let hide (e: #Dom_html.element t) =
        Eliom_lib.debug "hide !" ;
    let data = U.fun_call (U.variable "jQuery") [|U.inject e|] in
        let api = U.meth_call data "data" [| U.inject (string "tooltip") |] in
        ignore (U.meth_call api "hide" [| |])



        end

