open Eliom_lib
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.F

(** Quelques utilitaires generiques *)

let lclasse s = a_class [s]
let lclasses s = a_class s

let classe s = [a_class [s]]
let classes s = [a_class s]

let divc s l =
  div ~a:(classe s) l

let divcs s l =
  div ~a:(classes s) l

let spanc s l =
  span ~a:(classe s) l

let spancs s l =
  span ~a:(classes s) l

let center l =
  div ~a:(classe "text-center") l

let dummy_a ?(a=[]) content =
  Raw.a ~a:((a_href (uri_of_string (fun () ->"#"))) :: a) content
(** Des éléments bootstrap *)

(** Icons *)
let icon ?(a=[]) ?(white=false) s =
  let style = if white then ["icon-white"] else [] in
  i ~a:(a_class (("icon-"^s) :: style) :: a) []

let d_icon ?(a=[]) ?(white=false) s =
  let style = if white then ["icon-white"] else [] in
  i ~a:(a_class (("icon-"^s) :: style) :: a) []

(** Caret *)
let caret =
  spanc "caret" []

(** Les labels *)
module Label = struct

  let default ?(c=[]) content =
    spancs ("label"::c) content

  let success content = default~c:["label-success"] content
  let warning content = default~c:["label-warning"] content
  let important content = default~c:["label-important"] content
  let info content = default~c:["label-info"] content
  let inverse content = default~c:["label-inverse"] content

end

(** Les badges *)
module Badge = struct

  let default ?(c=[]) content =
    spancs ("badge"::c) content

  let success content = default ~c:["badge-success"] content
  let warning content = default ~c:["badge-warning"] content
  let important content = default ~c:["badge-important"] content
  let info content = default ~c:["badge-info"] content
  let inverse content = default ~c:["badge-inverse"] content

end

(** Le layout grille de bootstrap *)

let grid_content ?(postfix="") columns =
  let span i = Printf.sprintf "span%i" i in
  let aux (size,content) = divc (span size) content in
  let body = List.map aux columns in
  divc ("row"^postfix) body

let grid ?(postfix="") ?(head = []) ?size columns =
  let t = match size with
    | None -> List.fold_left (fun s (x,_) -> s + x) 0 columns
    | Some i -> i
  in
  divc ("row"^postfix) [
    divc (Printf.sprintf "span%i" t) (
      head @
        [grid_content ~postfix columns]
    )]

let container ?(postfix="") ?(head = []) columns =
  divc ("container"^postfix) (
    head @
      [grid_content ~postfix columns]
  )

let grid_fluid ?(head=[]) = grid ~postfix:"-fluid" ~size:12 ~head

let container_fluid ?(head=[]) = container ~postfix:"-fluid" ~head


(** {2 Collasping} *)
module Collapse = struct

  (** Data attribute to make an element toogle colapse another element. Take the id of the element to collapse whithout "#" *)
  let a_collapse id =
    [a_user_data "toggle" "collapse" ; a_user_data "target" ("#"^id)]

  (** Button that toogle-collapse the element with the given id *)
  let button ?(a=[]) id content =
    button
      ~a:(lclasses ["btn"] :: a_collapse id @ a)
      ~button_type:`Button content

  (** Div to encapsulate the element to collapse *)
  let div ?(a=[]) id content =
    div
      ~a:(a_id id :: lclasse "collapse" :: a)
      content

  let a ?(a=[]) id content =
    dummy_a ~a:(a_collapse id @ a) content
end

(** {2 Dropdown Menus} *)


module Dropdown = struct

  let a_dropdown = a_user_data "toggle" "dropdown"

  let wrap (elem: ?a:'a -> 'b) ?(a=[]) ?(right=false) title content =
    let menu_class = if right then ["pull-right"] else [] in
    [elem
       ~a:(
         a_class ["dropdown-toggle"] ::
           a_dropdown ::
           a)
       title  ;
     ul ~a:[a_class ("dropdown-menu"::menu_class)] content
    ]

  let a ?(right=false) title content =
    wrap
      Raw.a
      ~a:[a_class ["link"]]
      ~right title content



  let nav =
    let a_ = a in (fun ?(a=[]) ?(right=false) title content ->
        li ~a:((a_class ["dropdown"]) :: a) (a_ ~right title content))

  let btn ?(a=[]) ?(right=false) title content =
    wrap
      (button ~button_type:`Button)
      ~a:(a_class ["btn"] :: a)
      ~right title content

  let btngroup ?(a=[]) ?(right=false) title content =
    divc "btn-group" (btn ~a ~right title content)

end

(** {2 ToogleClass}
    Little utility class to toogle a class on a target when fireing events on an element.
*)
module type CLASS = sig
  val v : string
  val revert : bool
end


(** {2 Typeahead}
    http://twitter.github.io/bootstrap/javascript.html#typeahead
*)

let rec add_user_data aux = function
  | [] -> aux
  | (s,None) :: l -> add_user_data aux l
  | (s,Some x) :: l -> add_user_data
                         (a_user_data s x :: aux)
                         l

let opt_map f = function Some x -> Some (f x) | None -> None


module Popover_html = struct

  type position =
      Right | Left | Top | Bottom

  let str_position =
    let right = "right" in
    let left = "left" in
    let top = "top" in
    let bottom = "bottom" in
    function
    | Right -> right
    | Left -> left
    | Top -> top
    | Bottom -> bottom

  type trigger =
      Click | Hover | Focus | Manual

  let layout s position tip_title tip_content =
    div ~a:(classes
              (s :: (function Some x -> [str_position x] | None -> []) position))
      [ divc "arrow" [] ;
        h3 ~a:(classe (s ^ "-title")) tip_title ;
        divc (s ^ "-content") tip_content
      ]

end


(** {2 jQuery tools ToolTip }
    http://jquerytools.org/documentation/tooltip/index.html *)

module Tooltip_html = struct

  type pos_h = [ `Right | `Left | `Center ]

  type pos_v = [ `Top | `Bottom | `Center ]

  let str_pos_h =
    let right = "right" in
    let left = "left" in
    let center = "center" in
    function
    | `Right -> right
    | `Left -> left
    | `Center -> center

  let str_pos_v =
    let top = "top" in
    let bottom = "bottom" in
    let center = "center" in
    function
    | `Bottom -> bottom
    | `Top -> top
    | `Center -> center

  let str_pos (v,h) = str_pos_v v ^ " " ^ str_pos_h h

end




