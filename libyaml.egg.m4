(

(version "1.0.0")
(synopsis "A yaml parser based on libyaml")
(author "riku-ri")
(maintainer "riku-ri")
(category parsing)
(license "m4_license()")
;(test-dependencies test)
;(foreign-dependencies git libyaml)
(platform (or linux unix))

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
