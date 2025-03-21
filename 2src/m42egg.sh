 #!/usr/bin/env bash
 m4 -Dm4_libyaml_c_src="$(find src/libyaml/src/ -type f -name '*.c')" -Dm4_libyaml_c_object="$(find src/libyaml/src/ -type f -name '*.c' |  xargs -I__ echo '(c-object __ (source __) (csc-options "-C" "-D HAVE_CONFIG_H" "-C" "-I./include/." "-C" "-I./src/libyaml/include/."))')"
