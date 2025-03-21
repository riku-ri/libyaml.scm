(

(license "m4_license())"
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
