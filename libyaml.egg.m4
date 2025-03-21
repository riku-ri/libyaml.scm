(

(components
m4_libyaml_c_object()
(extension libyaml
(source src/yaml.scm)
(objects
m4_libyaml_c_src()
)
(csc-options "-I" "./include/.")
(linkage dynamic)
)
)

)
