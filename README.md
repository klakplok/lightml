**lightml** (pronounce lighty-m-l) is an OCaml API to construct well formed static Web sites.
It main features are:
- Minimalist design, useable for writing a static site or as a back-end
- Concise, combinator based document writing
- Validity of HTML output (checked at typing time)
- Unicity of IDs inside every page (checked at output time)
- Absence of broken links (internal links checked at typing time, external ones at output time)

**Read this:** LigHTML is for simple usecases, such as adding an HTML
output back-end to an existing program. It provides interesting
guarantees that can shortcut a lot of testing and debugging but its
simplistic interface hides a lot of subtelties. For any advanced
purpose, such as building dynamic Web sites, you may want to use TyXML
instead [http://ocsigen.org/tyxml/manual].

**License:** LightML is (C) 2013 Benjamin Canou and distributed under the terms
of the CeCILL-B Free Software License Agreement. See the LICENSE
file for more details. 
