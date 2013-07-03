open Printf

type page = P of (string * untyped_node list ref * string * string list ref)
and untyped_node = S of string | T of descr * untyped_node list
and untyped_nodes = untyped_node list
and descr = { tag : string ; attrs : attr list }
and attr = A of string * attr_value | Href of page * string option
and attr_value = Raw of string | Seq of string list | Url of url
and url = Local of string * string option * bool | Extern of string * string option

type 'a node = untyped_node
type 'a nodes = untyped_nodes

exception Duplicate_path of string
exception Duplicate_id of string
exception Unbound_id of string * string
exception Unbound_url of string

let local ?(probe = true) ?target url =
  Local (url, target, probe)

let extern ?target url =
  Extern (url, target)

let rec collect_ids acc = function
  | S _ -> acc
  | T ({ attrs }, l) ->
    let acc = List.fold_left collect_ids acc l in
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

module Paths = struct
  include Set.Make (String)
  let paths = ref empty
  let use p =
    if mem p !paths then (
      raise (Duplicate_path p)
    ) else (
      paths := add p !paths ;
      p
    )
      
  let gpuid = ref 0
  let gen () =
    incr gpuid ;
    sprintf "gen%06X.html" !gpuid
end

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

let text s = S (esc s)
let cdata s = S ("<![CDATA[" ^ s ^ "]]>")
let comment s = S ("<!--" ^ s ^ "-->")
let nbsp () = S "&nbsp;"
let elt tag ?(attrs = []) children = 
  T ({ tag ; attrs }, children)

let builder tag ?id ?(cls = []) children =
  let attrs =
    (match id with None -> [] | Some c -> [A ("id", Raw c)])
    @ (match cls with [] -> [] | l -> [A ("class", Seq l)])
  in
  elt tag ~attrs children

let br () = elt "br" []
let blockquote = builder "blockquote"
let span = builder "span"
let div = builder "div"
let article = builder "article"
let section = builder "section"
let pre ?id ?(cls = []) t = builder "pre" ?id ~cls [t]
let table ?id ?(cls = []) ?headers t =
  let t = match headers with None -> t | Some ths -> (builder "tr" ths) :: t in
  if t = [] then invalid_arg "LigHTML.table" ;
  builder ?id ~cls "table" t
let tr ?id ?(cls = []) ?header t =
  let t = match header with None -> t | Some th -> th :: t in
  if t = [] then invalid_arg "LigHTML.tr" ;
  builder ?id ~cls "tr" t
let td = builder "td"
let th = builder "th"
let ul = builder "ul"
let ol = builder "ol"
let li = builder "li"
let h lvl =
  if lvl < 1 || lvl > 6 then invalid_arg "LigHTML.h" ;
  builder ("h" ^ string_of_int lvl)
let h1 = builder "h1"
let h2 = builder "h2"
let h3 = builder "h3"
let h4 = builder "h4"
let h5 = builder "h5"
let h6 = builder "h6"
let sub = builder "sub"
let sup = builder "sup"
let generic tag ?id ?(cls = []) ?(attrs = []) children =
  let attrs =
    (match id with None -> [] | Some c -> [A ("id", Raw c)])
    @ (match cls with [] -> [] | l -> [A ("class", Seq l)])
    @ (List.map (fun (n, v) -> A (n, Raw v)) attrs)
  in
  elt tag ~attrs children
let inline = generic
let block = generic

let img ?id ?(cls = []) url =
  let attrs =
    (match id with None -> [] | Some c -> [A ("id", Raw c)])
    @ (match cls with [] -> [] | l -> [A ("class", Seq l)])
    @ ([A ("src", Url url)])
  in
  elt "img" ~attrs []

let a_url ?id ?(cls = []) url children =
  let attrs =
    (match id with None -> [] | Some c -> [A ("id", Raw c)])
    @ (match cls with [] -> [] | l -> [A ("class", Seq l)])
    @ ([A ("href", Url url)])
  in
  elt "a" ~attrs children

let a ?id ?(cls = []) ?target page children =
  let cls = "local-url" :: cls in
  let attrs =
    (match id with None -> [] | Some c -> [A ("id", Raw c)])
    @ (match cls with [] -> [] | l -> [A ("class", Seq l)])
    @ ([Href (page, target)])
  in
  elt "a" ~attrs children

let new_page ?(path = Paths.gen ()) ?(contents = []) title : page =
  P (title, ref contents, Paths.use path, ref [])

let mkdirs basedir path =
  ((* TODO *))

let write ?(basedir = ".") (p : page) =
  let written = ref [] in
  let to_write = ref [p] in
  let do_page title contents path =
    let buf = Buffer.create 100_000 in
    let lvl = ref 0 in
    let indent buf n =
      for i = 1 to if n > 0 then !lvl else !lvl + n do Buffer.add_char buf ' ' done ;
      lvl := !lvl + n
    in
    let bprint_attrs buf =
      let target = function | None -> "" | Some t -> "#" ^ t in
      List.iter (function
	| A (a, Raw v) -> bprintf buf " %s='%s'" a v
	| A (a, Seq []) -> ()
	| A (a, Seq (e :: tl)) -> bprintf buf " %s='%s'" a (List.fold_left (fun r s -> r ^ " " ^ s) e tl)
	| A (a, Url (Local (v, t, false))) -> bprintf buf " %s='%s%s'" a v (target t)
	| A (a, Url (Local (v, t, true))) ->
	  if not (Sys.file_exists (basedir ^ "/" ^ v)) then raise (Unbound_url v) ;
	  bprintf buf " %s='%s%s'" a v (target t)
	| A (a, Url (Extern (v, t))) -> bprintf buf " %s='%s%s'" a v (target t)
	| Href (P (_, _, path, _) as p, None) ->
	  if not (List.memq p !written) && not (List.memq p !to_write) then
	    to_write := p :: !to_write ;
	  bprintf buf " href='%s'" path
	| Href (P (_, _, path, tids) as p, Some id) ->
	  if not (List.mem id !tids) then
	    raise (Unbound_id (path, id)) ;
	  if not (List.memq p !written) && not (List.memq p !to_write) then
	    to_write := p :: !to_write ;
	  bprintf buf " href='%s#%s'" path id
      ) in
    let rec aplat = function
      | S s -> true
      | T (_, [S s]) -> true
      | T (_, l) when List.exists (function | S _ | T({ tag = ""},_) -> true | _ -> false) l -> true
      | T (_, [T (_, [S s])]) when String.length s < 30 -> true
      | _ -> false
    in
    let rec aux a = function
      | S s ->
	if a then
	  bprintf buf "%s" s
	else
	  bprintf buf "%a%s\n" indent 0 s
      | T ({ tag ; attrs }, []) ->
	if a then
	  bprintf buf "<%s%a></%s>" tag bprint_attrs attrs tag
	else
	  bprintf buf "%a<%s%a></%s>\n" indent 0 tag bprint_attrs attrs tag
      | T ({ tag ; attrs }, l) as e ->
	let ae = a || aplat e in
	if a then (
	  bprintf buf "<%s%a>" tag bprint_attrs attrs ;
	  List.iter (aux true) l ;
	  bprintf buf "</%s>" tag
	) else (
	  if ae then (
	    bprintf buf "%a<%s%a>" indent 0 tag bprint_attrs attrs ;
	    List.iter (aux true) l ;
	    bprintf buf "</%s>\n" tag
	  ) else (
	    bprintf buf "%a<%s%a>\n" indent 1 tag bprint_attrs attrs ;
	    List.iter (aux false) l ;
	    bprintf buf "%a</%s>\n" indent (-1) tag
	  )
	)
    in
    let e = elt "html" [
      elt "head" [
	elt "meta" ~attrs:[A ("http-equiv", Raw "content-type") ; A ("content", Raw "text/html; charset=utf-8")] [] ;
	elt "meta" ~attrs:[A ("name", Raw "robots") ; A ("content", Raw "index, follow")] [] ;
	elt "meta" ~attrs:[A ("name", Raw "generator") ; A ("content", Raw "LigHTML")] [] ;
	elt "link" ~attrs:[A ("rel", Raw "stylesheet") ; A ("type", Raw "text/css") ; A ("href", Raw "style.css")] [] ;
	elt "script" ~attrs:[A ("type", Raw "text/javascript") ; A ("src", Raw "script.js")] [] ;
	elt "title" [ text title ]
      ] ;
      elt "body" !contents
    ] in
    aux false e ;
    let t = Buffer.contents buf in
    mkdirs basedir path ;
    let fp = open_out (basedir ^ "/" ^ path) in
    output_string fp "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" ;
    output_string fp t ;
    close_out fp
  in
  while !to_write <> [] do
    let P (title, contents, path, _) as p = List.hd !to_write in
    written := p :: !written ;
    to_write := List.tl !to_write ;
    do_page title contents path
  done
    
let set_page_contents (P (_, contents, _, l) : page) e =
  contents := e ;
  l := collect_ids [] (T ({ tag = "" ; attrs = [] }, e))
