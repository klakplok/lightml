lightml
=======

A minimalist OCaml API to construct well formed static Web sites
- Checks the validity of HTML output (at typing time)
- Checks the unicity of IDs inside every page (at output time)
- Checks the absence of broken links (internal at typing time and external at output time)

**Read this:** LigHTML is for simple usecases, such as adding an HTML
output back-end to an existing program. It provides interesting
guarantees that can shortcut a lot of testing and debugging but its
simplistic interface hides a lot of subtelties. For any advanced
purpose, such as building dynamic Web sites, you may want to use TyXML
instead [http://ocsigen.org/tyxml/manual].

**License:** LightML is (C) 2013 Benjamin Canou and distributed under the terms
of the CeCILL-B Free Software License Agreement. See the LICENSE
file for more details. 
