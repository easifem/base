project: EASIFEM
project_dir: ./src
project_github: https://gitlab.com/vickysharma0812
project_website: https://gitlab.com/vickysharma0812
media_dir: ./examples
page_dir: ./pages
output_dir: ./doc
exclude_dir: ./src/Base
             ./src/scripts
             ./src/Extpkgs
author: Dr Vikas Sharma
author_description: Postdoctoral fellow
	Indian Institute of Technology Bombay
	Mumbai, India
email: vickysharma0812@gmail.com
github: https://gitlab.com/vickysharma0812
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

FORD features two macros to make it easier to provide intradocumentation links. These are `|url|` which gets replaced by the project URL, and `|media|`, which gets replaced by the (absolute) path to the media directory in the output. you can also use `favicon:`