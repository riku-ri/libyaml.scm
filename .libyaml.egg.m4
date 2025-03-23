(

(version "1.0.0")
(synopsis "A yaml parser based on libyaml")
(author "m4_author()")
(maintainer "m4_author()")
(category parsing)
(license "m4_license()")
;(dependencies)
;(test-dependencies test)
;(build-dependencies)
;(foreign-dependencies git)
(platform (or linux unix))
;((distribution-files)

(components
m4_libyaml_c_object()
(extension libyaml
(source src/yaml.scm)
(objects
m4_libyaml_c_src()
)
(csc-options "-I" "./include/." "-C" "-I./src/libyaml/include/.")
(linkage dynamic)
)
)

)
