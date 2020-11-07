project_dir: ./src/modules/
project_github: https://github.com/vickysharma0812/easifem
project_website: https://vickysharma0812.github.io/easifem/
media_dir: ./easifem-tutorials
page_dir: ./pages
output_dir: ./docs
exclude_dir: ./src/Extpkgs
             ./src/submodules
             ./src/scripts
author: Dr Vikas Sharma
author_description: Ph. D.
	Kyoto University
	Kyoto, Japan
email: vickysharma0812@gmail.com
github: https://vickysharma0812.github.io/
summary: Expandable and Scalable Infrastructure for Finite Element Methods
graph: false
source: true
display: public
         protected
         private
page: true
sort: alpha
coloured_edges: true
extra_filetypes: .inc !
extra_filetypes: .part !
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
predocmark_alt: >
predocmark: <
docmark_alt: *
docmark: !
fpp_extensions: f90
preprocesses: true

<!-- {!./README.md!} -->

<!-- FORD features two macros to make it easier to provide intradocumentation links. These are `|url|` which gets replaced by the project URL, and `|media|`, which gets replaced by the (absolute) path to the media directory in the output. you can also use `favicon:` -->