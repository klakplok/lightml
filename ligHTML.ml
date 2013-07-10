(* This file is part of LightML and is (C) 2013 Benjamin Canou. It is
   distributed under the terms of the CeCILL-B Free Software License
   Agreement. See the LICENSE file for more details. *)

(* exported (abstract) types *)
type page = P of (string * untyped_node list ref * untyped_node list * string * string list ref)
and url = Local of string * string option * bool | Extern of string * string option

(* internal types *)
and descr = { tag : string ; attrs : attr list }
and attr = A of string * attr_value | Href of page * string option
and attr_value = Raw of string | Seq of string list | Url of url
and untyped_node = S of string | T of descr * untyped_node list
and untyped_nodes = untyped_node list

(* exported types (with a phantom parameter to encode valid nesting) *)
type 'a node = untyped_node
type 'a nodes = untyped_nodes
type meta = untyped_node

(* exported exceptions *)
exception Duplicate_path of string (* path *)
exception Duplicate_id of string (* id *)
exception Unbound_id of string * string (* path x id *)
exception Unbound_url of string (* url *)

(* tools ********************************************************************)

let use_path =
  (* TODO: normalize (remove ..s) *)
  let module Store = Set.Make (String) in
  let paths = ref Store.empty in
  fun p ->
    if Store.mem p !paths then raise (Duplicate_path p) ;
    paths := Store.add p !paths ; p

let gen_path =
  let gpuid = ref (-1) in
  fun () ->
    incr gpuid ;
    Printf.sprintf "gen%06X.html" !gpuid

let collect_and_check_ids root =
  let rec traverse acc = function
    | S _ -> acc
    | T ({ attrs }, l) ->
      let acc = List.fold_left traverse acc l in
      let rec find = function
	| A ("id", Raw id) :: tl ->
	  if List.mem id acc then
	    raise (Duplicate_id id)
	  else
	    id :: acc
	| A _ :: tl ->
	  find tl
	| Href _ :: tl -> find tl
	| [] -> acc
      in find attrs
  in traverse [] root

let esc s =
  let buf = Buffer.create 1000 in
  let spaces = ref 0 in
  let put = function
    | " " -> incr spaces
    | s ->
      if !spaces <> 0 then
	Buffer.add_char buf ' ' ;
      Buffer.add_string buf s ;
      spaces := 0
  in
  for i = 0 to String.length s - 1 do
    match s.[i] with 
      | '\n' -> put " "
      | '\240' -> put "&nbsp;"
      | '<' -> put "&lt;"
      | '>' -> put "&gt;"
      | '&' -> put "&amp;"
      | c -> put (String.make 1 c)
  done ;
  if !spaces <> 0 then
    Buffer.add_char buf ' ' ;
  Buffer.contents buf

let mkdirs basedir path =
  ((* TODO *))


(* url builders *************************************************************)

let local ?(probe = true) ?target url = Local (url, target, probe)
let extern ?target url = Extern (url, target)

(* text node builders *******************************************************)

let text s = S (esc s)
let cdata s = S ("<![CDATA[" ^ s ^ "]]>")
let comment s = S ("<!--" ^ s ^ "-->")
let nbsp () = S "&nbsp;"

(* generic element node builders ********************************************)

let elt tag ?(attrs = []) children = 
  T ({ tag ; attrs }, children)

let builder ?(attrs = []) tag ?id ?(cls = []) children =
  let attrs =
    (match id with None -> [] | Some c -> [A ("id", Raw c)])
    @ (match cls with [] -> [] | l -> [A ("class", Seq l)])
    @ attrs
  in
  elt tag ~attrs children

let generic tag ?id ?(cls = []) ?(attrs = []) children =
  let attrs = List.map (fun (n, v) -> A (n, Raw v)) attrs in
  builder ~attrs tag ?id ~cls children
    
let inline = generic and block = generic

(* element node builders ****************************************************)

let br () = elt "br" []
let blockquote = builder "blockquote"
let div = builder "div" and span = builder "span"
let article = builder "article" and section = builder "section"
let pre ?id ?(cls = []) t = builder "pre" ?id ~cls [t]
let ul = builder "ul" and ol = builder "ol" and li = builder "li"
let h lvl =
  if lvl < 1 || lvl > 6 then invalid_arg "LigHTML.h" ;
  builder ("h" ^ string_of_int lvl)
let h1 = h 1 and h2 = h 2 and h3 = h 3 and h4 = h 4 and h5 = h 5 and h6 = h 6
let sub = builder "sub" and sup = builder "sup"
let img ?id ?(cls = []) url =
  builder ~attrs:[A ("src", Url url)] "img" ?id ~cls []
let a_url ?id ?(cls = []) url children =
  builder ~attrs:[A ("href", Url url)] "a" ?id ~cls children
let a ?id ?(cls = []) ?target page children =
  let cls = "lightml-page-ref" :: cls in
  builder ~attrs:[Href (page, target)] "a" ?id ~cls children

(* table builders ***********************************************************)

let table ?id ?(cls = []) ?headers t =
  let t = match headers with None -> t | Some ths -> (builder "tr" ths) :: t in
  if t = [] then invalid_arg "LigHTML.table" ;
  builder ?id ~cls "table" t
let tr ?id ?(cls = []) ?header t =
  let t = match header with None -> t | Some th -> th :: t in
  if t = [] then invalid_arg "LigHTML.tr" ;
  builder ?id ~cls "tr" t
let td = builder "td"
and th = builder "th"

(* meta builders ************************************************************)


let style url =
  elt "link" ~attrs:[A ("rel", Raw "stylesheet") ;
		     A ("type", Raw "text/css") ;
		     A ("href", Url url)] []
let script url =
  elt "script" ~attrs:[A ("type", Raw "text/javascript") ; A ("src", Url url)] []
let inline_style code =
  elt "style" [ cdata code ]
let inline_script code =
  elt "script" ~attrs:[A ("type", Raw "text/javascript")] [ cdata code ]

(* main functions ***********************************************************)

let new_page ?path ?(metadata = []) ?(contents = []) title : page =
  let path = match path with None -> gen_path () | Some p -> p in
  P (title, ref contents, metadata, use_path path, ref [])
    
let set_page_contents (P (_, contents, _, _, l) : page) e =
  contents := e ;
  l := collect_and_check_ids (elt "fake" e)

let write ?(basedir = ".") (p : page) =
  let written = ref [] in
  let to_write = ref [p] in
  let do_page title contents metadata path =
    let buf = Buffer.create 100_000 in
    let lvl = ref 0 in
    let indent buf n =
      for i = 1 to if n > 0 then !lvl else !lvl + n do Buffer.add_char buf ' ' done ;
      lvl := !lvl + n
    in
    let do_attrs buf =
      let target = function | None -> "" | Some t -> "#" ^ t in
      List.iter (function
	| A (a, Raw v) -> Printf.bprintf buf " %s='%s'" a v
	| A (a, Seq []) -> ()
	| A (a, Seq (e :: tl)) -> Printf.bprintf buf " %s='%s'" a (List.fold_left (fun r s -> r ^ " " ^ s) e tl)
	| A (a, Url (Local (v, t, false))) -> Printf.bprintf buf " %s='%s%s'" a v (target t)
	| A (a, Url (Local (v, t, true))) ->
	  if not (Sys.file_exists (basedir ^ "/" ^ v)) then raise (Unbound_url v) ;
	  Printf.bprintf buf " %s='%s%s'" a v (target t)
	| A (a, Url (Extern (v, t))) -> Printf.bprintf buf " %s='%s%s'" a v (target t)
	| Href (P (_, _, _, path, _) as p, None) ->
	  if not (List.memq p !written) && not (List.memq p !to_write) then
	    to_write := p :: !to_write ;
	  Printf.bprintf buf " href='%s'" path
	| Href (P (_, _, _, path, tids) as p, Some id) ->
	  if not (List.mem id !tids) then
	    raise (Unbound_id (path, id)) ;
	  if not (List.memq p !written) && not (List.memq p !to_write) then
	    to_write := p :: !to_write ;
	  Printf.bprintf buf " href='%s#%s'" path id
      ) in
    let rec inlineable = function
      | S s -> true
      | T (_, [S s]) -> true
      | T (_, l) when List.exists (function | S _ | T({ tag = ""},_) -> true | _ -> false) l -> true
      | T (_, [T (_, [S s])]) when String.length s < 30 -> true
      | _ -> false
    in
    let rec do_node ?(inline = false) = function
      | S s ->
	if inline then
	  Printf.bprintf buf "%s" s
	else
	  Printf.bprintf buf "%a%s\n" indent 0 s
      | T ({ tag ; attrs }, []) ->
	if inline then
	  Printf.bprintf buf "<%s%a></%s>" tag do_attrs attrs tag
	else
	  Printf.bprintf buf "%a<%s%a></%s>\n" indent 0 tag do_attrs attrs tag
      | T ({ tag ; attrs }, l) as e ->
	if inline then (
	  Printf.bprintf buf "<%s%a>" tag do_attrs attrs ;
	  List.iter (do_node ~inline:true) l ;
	  Printf.bprintf buf "</%s>" tag
	) else if inlineable e then (
	  Printf.bprintf buf "%a<%s%a>" indent 0 tag do_attrs attrs ;
	  List.iter (do_node ~inline:true) l ;
	  Printf.bprintf buf "</%s>\n" tag
	) else (
	  Printf.bprintf buf "%a<%s%a>\n" indent 1 tag do_attrs attrs ;
	  List.iter do_node l ;
	  Printf.bprintf buf "%a</%s>\n" indent (-1) tag
	)
    in
    let root = elt "html" [
      (* TODO: customize headers *)
      elt "head" ([
	elt "meta" ~attrs:[A ("http-equiv", Raw "content-type") ; A ("content", Raw "text/html; charset=utf-8")] [] ;
	elt "meta" ~attrs:[A ("name", Raw "robots") ; A ("content", Raw "index, follow")] [] ;
	elt "meta" ~attrs:[A ("name", Raw "generator") ; A ("content", Raw "LigHTML")] [] ;
	elt "title" [ text title ]
      ] @ metadata) ;
      elt "body" contents
    ] in
    do_node root ;
    let t = Buffer.contents buf in
    mkdirs basedir path ;
    let fp = open_out (basedir ^ "/" ^ path) in
    output_string fp "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" ;
    output_string fp t ;
    close_out fp
  in
  while !to_write <> [] do
    let P (title, contents, metadata, path, _) as p = List.hd !to_write in
    written := p :: !written ;
    to_write := List.tl !to_write ;
    do_page title !contents metadata path
  done
