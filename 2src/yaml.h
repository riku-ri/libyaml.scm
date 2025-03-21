#include <stddef.h>
typedef size_t size_t;
/* From riku.ri@outlook.com
	In my case(Arch Linux clang19),
	libclang will parse size_t to int directly.
	To detach size_t and keep it,
	here typedef size_t to make size_t always a typedef but not compiler built-in type.
*/

#include "src/libyaml/include/yaml.h"
