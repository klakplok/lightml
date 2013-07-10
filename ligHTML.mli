(** A minimalist API to construct well formed static Web sites
    - Checks the validity of HTML output (at typing time)
    - Checks the unicity of IDs inside every page (at output time)
    - Checks the absence of broken links (internal at typing time and external at output time)

    To build a Web site, simply build all your pages using {!new_page} and
    then call {!write} on the root of your site.

    LightML is (C) 2013 Benjamin Canou and distributed under the terms
    of the CeCILL-B Free Software License Agreement. See the LICENSE
    file for more details. *)

(** {2 Types}. *)

(** The type of HTML nodes (both elements and text). The type
    parameter is a phantom type (most of the time a subtype of [[
    `TEXT | `INLINE | `BLOCK ]]) used to restrict the inputs and
    output of node builders to ensure correct nesting of nodes and
    thus validity. For instance, it is impossible to include block
    elements inside inline elements, a link into a link or anything
    but a list item in a list. *)
type +'a node

(** The type of a single HTML document, see {!new_page} and
    {!write}. Values of this type are used as target for {!a}
    nodes so no broken links can be created. *)
type page

(** The type of external to the lightml site URLs. Used both for
    {!a_url} nodes or non-HTML objects such as {!img}.*)
type url

(** The type of meta headers to feed to {!new_page} *)
type meta

(** {2 Site Building and Writing}. *)

(** The argument is the path itself. *)
exception Duplicate_path of string

(** The argument is the ID itself. *)
exception Duplicate_id of string

(** The first component if the path, the second is ths ID. *)
exception Unbound_id of string * string

(** The argument is the URL itself. *)
exception Unbound_url of string

(** Builds a new HTML doc. If [path] contains slashes, directories
    will be created on demand by {!write}. A {!Duplicate_path}
    exception is raised if [path] has already been used for another
    document. The return values can be used as the root of the website
    to output or as link targets (see {!a}). To implement cyclic
    links, you can first build a fake empty document and update it
    later using {!set_page_contents}. A {!Duplicate_id} exception will
    be raised if a page contains som ID more than once. *)
val new_page :
  ?path:string ->
  ?metadata:meta list ->
  ?contents:[< `BLOCK | `INLINE | `TEXT ] node list ->
  string -> page

(** Updates the content of a document. A {!Duplicate_id} exception will
    be raised if a page contains som ID more than once. *)
val set_page_contents : page -> [< `BLOCK | `INLINE | `TEXT ] node list -> unit

(** Flushes the website from the given root. Only pages accessible
    from this root will be written. The default [basedir] is the
    current directory. The default [charset] is ["utf-8"]. An
    {!Unbound_url} exception will be raised if a probed reference to a
    non existing local URL is encountered. A {!Unbound_id} exception
    will be raised if a targeted {!a} link is unbound. *)
val write : ?basedir:string -> ?charset:string -> page -> unit

(** Build an internal URL. If [probe] is [true] (default), a check is
    performed during {!write} to ensure that a local file exists
    at the given path. This check takes into account the specified
    [basedir]. *)
val local : ?probe:bool -> ?target:string -> string -> url

(** Build an external URL.
    @param probe: if [true], checks that a local file exists. *)
val extern :  ?target:string -> string -> url

(** {2 HTML content creation}. *)

(** {3 Basic Text Blocks}. *)

(** Raw (verbatim) text noode. *)
val text : string -> [> `TEXT ] node

(** Xml comment node. *)
val comment : string -> [> `TEXT ] node

(** Text node with escaped entities (ex. > becomes &lt;). *)
val cdata : string -> [> `TEXT ] node

(** Non breaking space (&nbsp;) text node. *)
val nbsp : unit -> [> `TEXT ] node

(** Image from its URL  (see {!local} and {!extern}). *)
val img : ?id:string -> ?cls:string list -> url -> [> `TEXT ] node

(** {3 Inline Layout}. *)

(** Inline group of inline elements. *)
val span : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `INLINE ] node

(** Subscript text. *)
val sub : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `INLINE ] node

(** Superscript text. *)
val sup : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `INLINE ] node

(** Builds an internal link. A specific part of the destination can be
   targeted by putting an ID in it and passing this ID as parameter
   [target]. *)
val a : ?id:string -> ?cls:string list -> ?target:string -> page -> [< `TEXT ] node list -> [> `INLINE ] node

(** Builds a link to an external URL (see {!local} and {!extern}). *)
val a_url : ?id:string -> ?cls:string list -> url -> [< `TEXT ] node list -> [> `INLINE ] node

(** Generic inline node, the first argument is the tag. *)
val inline : string -> ?id:string -> ?cls:string list -> ?attrs:(string * string) list -> [< `INLINE | `TEXT] node list -> [> `INLINE ] node

(** {3 Headings}. *)

(** An HTML heading. The first argument is the level between 1 and 6
    (otherwise an [Invalid_argument] exception is raised). *)
val h : int -> ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node

val h1 : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node
val h2 : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node
val h3 : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node
val h4 : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node
val h5 : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node
val h6 : ?id:string -> ?cls:string list -> [< `INLINE | `TEXT ] node list -> [> `BLOCK ] node

(** {3 Block Layout}. *)

(** A line break. *)
val br : unit -> [> `BLOCK ] node

(** A block element. *)
val div : ?id:string -> ?cls:string list -> [< `INLINE | `BLOCK | `TEXT] node list -> [> `BLOCK ] node

(** A block element for quotations. *)
val blockquote : ?id:string -> ?cls:string list -> [< `INLINE | `BLOCK | `TEXT] node list -> [> `BLOCK ] node

(** A block element that contains verbatim text. *)
val pre : ?id:string -> ?cls:string list -> [< `TEXT ] node -> [> `BLOCK ] node

(** A structurating section block element. *)
val section : ?id:string -> ?cls:string list -> [< `INLINE | `BLOCK | `TEXT] node list -> [> `BLOCK ] node

(** A structurating article block element. *)
val article : ?id:string -> ?cls:string list -> [< `INLINE | `BLOCK | `TEXT] node list -> [> `BLOCK ] node

(** Generic block node, the first argument is the tag. *)
val block : string -> ?id:string -> ?cls:string list -> ?attrs:(string * string) list -> [< `INLINE | `BLOCK | `TEXT] node list -> [> `BLOCK ] node

(** {3 Tables}. *)

(** Builds a table from a sequence of rows constructed by {!tr} (at
    least one row must be given otherwise an [Invalid_argument]
    exception will be raised). *)
val table : ?id:string -> ?cls:string list -> ?headers:[< `TH ] node list -> [< `TR ] node list -> [> `BLOCK ] node

(** Builds a table row from a sequence of cells constructed by {!td}
    (at least one cell must be given otherwise an [Invalid_argument]
    exception will be raised). *)
val tr : ?id:string -> ?cls:string list -> ?header:[< `TH ] node -> [< `TD ] node list -> [> `TR ] node

(** A table cell. *)
val td : ?id:string -> ?cls:string list -> [< `BLOCK | `INLINE | `TEXT ] node list -> [> `TD ] node

(** A table header cell. *)
val th : ?id:string -> ?cls:string list -> [< `BLOCK | `INLINE | `TEXT ] node list -> [> `TH ] node

(** {3 Lists}. *)

(** An unordered (bullet) list. *)
val ul : ?id:string -> ?cls:string list -> [< `LI ] node list -> [> `BLOCK ] node

(** An enumerated list. *)
val ol : ?id:string -> ?cls:string list -> [< `LI ] node list -> [> `BLOCK ] node

(** A list item both for {!ul} or {!ol}. *)
val li : ?id:string -> ?cls:string list -> [< `BLOCK | `INLINE | `TEXT ] node list -> [> `LI ] node

(** {2 Meta Information}. *)

(** A JavaScript file *)
val script: url -> meta

(** A CSS file *)
val style: url -> meta

(** A JavaScript code block *)
val inline_script: string -> meta

(** A CSS code block *)
val inline_style: string -> meta

