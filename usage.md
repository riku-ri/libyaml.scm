# libyaml.scm

This is a chicken scheme egg,
section structure will follow
- [http://wiki.call-cc.org/eggs%20tutorial#sections](http://wiki.call-cc.org/eggs%20tutorial#sections).

## Authors

- [riku-ri@outlook.com](riku-ri@outlook.com)

## Repository

[https://github.com/riku-ri/libyaml.scm/](https://github.com/riku-ri/libyaml.scm/)

## Requirements

## API

### Values in `yaml.h`

[libyaml.scm<sub>git</sub>](.) will
export enum members and functions in
[*yaml.h*](https://github.com/yaml/libyaml/blob/master/include/yaml.h) from
[yaml/libyaml<sub>git</sub>](https://github.com/yaml/libyaml).

Functions and enum members can be used in scheme code directly.

> Note that you may install libyaml development package in your system,
> but [libyaml.scm<sub>git</sub>](.) will use the *yaml.h* in
> submodule *src/libyaml* but not the yaml.h in your system

### Read yaml file or string

```
(yaml<- [ARGUMENTS]) --> SCHEME-YAML-OBJECT
```

#### `SCHEME-YAML-OBJECT`
- yaml content will be parse to a scheme-list of yaml-document,
	and each document will be a nested structure of them :
	- yaml-list will be a scheme-list
		- yaml `[1 , 2]`  
		=> scheme `(list 1 2)`
	- yaml-mapping will be a procedure that generate a "association list"
		- yaml `{key: value}`  
		=> scheme `(lambda () (list (cons "key" "value")))`
	- atom data  
		Atom datas are divided according to the *Tag Resolution section* of
		[yaml 1.2 spec](https://yaml.org/spec/1.2.2/) :
		- yaml `null`  
		=> scheme empty list, *i.e.* `(list)` or `'()`
		- yaml `true` `false`  
		=> scheme `#t` `#f`
		- nunmbers :
			- yaml `1` `0.5` `0o10` `0x10` `10e1` `3E2`  
			=> scheme `1` `0.5` `8` `16` `100.0` `300.0`
			- yaml `+.inf` `-.INF`  
			=> scheme `+inf.0` `-inf.0`
			- yaml `.nan`  
			=> scheme `+nan.0`
		- others will be treated as literal string, including any date format

##### Examples

```yaml
---
- .NaN
- -.inf
---
string
...
```

will be

```
(list (list +nan.0 -inf.0) "string")
```

---

```yaml
a: b
c:
  - 1
  - 2
d:
  e: f
g:
```

will be

```lisp
(list
	(lambda ()
		(list
			(cons "a" "b")
			(cons "c" (list 1 2))
			(cons "d" (lambda () (list (cons "e" "f"))))
			(cons "g" (list))
		)
	)
)
```

#### Arguments

- `'(#:input . INPUT)`  
	`INPUT` should be a input port or yaml string.  
	If not set, `yaml<-` will read yaml from `(current-input-port)`.

	> `yaml<-` will auto close the input port if finished or failed.

- `'(#:encoding . ENCODING)`  
	Encoding of the yaml file or yaml string.
	The value should be one of
	`YAML_ANY_ENCODING`
	`YAML_UTF8_ENCODING`
	`YAML_UTF16LE_ENCODING`
	`YAML_UTF16BE_ENCODING` .
	That is the element of `enumml_encoding_e` defined in the C language header file
	`yaml.h` from [yaml/libyaml](https://github.com/yaml/libyaml) .  
	If not set, this will be `YAML_ANY_ENCODING` .

##### Examples

```lisp
(import libyaml)

(write (yaml<-)) ; generally, this will read from stdin
```

> For example, after saving above code in a file named `tmp.scm`,
> call The CHICKEN Scheme interpreter `csi` :  
> `echo '[a,b,c]' | csi -s tmp.scm`  
> will print `(("a" "b" "c"))`

---

```lisp
(import yaml)
(write (yaml<- `(#:input . ,(open-input-file "/tmp/tmp.yaml"))))
```

> You need to create file `/tmp/tmp.yaml` and write yaml content to it

---

```lisp
(import libyaml)

(map print (list
(yaml<- (cons #:encoding YAML_UTF8_ENCODING) '(#:input . "--- string"))
(yaml<- '(#:input . "--- another string") `(#:encoding . ,YAML_ANY_ENCODING))
))
```

### Dump yaml object

```
(<-yaml SCHEME-YAML-OBJECT [ARGUMENTS])
```

#### Arguments

- `SCHEME-YAML-OBJECT` *necessary*  
	The first argument to `<-yaml` must be a `SCHEME-YAML-OBJECT`.  
	Generally this may be the result of `yaml<-`,
	you can also construct a object manually follow the above structure definition in
	*Read yaml file or string* section.
- `'(#:indent . INDENT)`  
	`INDENT` should be a integer, the indent size in the output yaml file
- `'(#:port . PORT)`  
	`PORT` should be a output port to write.  
	If not set,  `<-yaml` will write to `(current-output-port)` .

	> `<-yaml` will auto close the output port if finished or failed.

- `'(#:encoding . ENCODING)`  
	The same as `#:encoding` option in `yaml<-` .

##### Examples

```lisp
(import libyaml)

(let ((yaml (yaml<- '(#:input . "[a, b, c]"))))
	(<-yaml yaml) ; generally, this will print to screen
	(<-yaml yaml `(#:port . ,(open-output-file "/tmp/tmp.yaml"))) ; output to /tmp/tmp.yaml
	; Note that do not quote the port
	(<-yaml yaml '(#:indent . 4) `(#:encoding . ,YAML_UTF8_ENCODING))
)
```

### More about yaml-mapping

#### Show the yaml-mapping

As definition in *Read yaml file or string* section,
yaml-mapping will be a procedure that generate a "association list".
So `print` `display` them will just show a hided procedure but not its content.
`mapping-ordered-yaml<-` will recursively sort a `SCHEME-YAML-OBJECT` and
replace procedure to its result.

```
(mapping-ordered-yaml<- SCHEME-YAML-OBJECT [ARGUMENTS])
```

Arguments :
- `SCHEME-YAML-OBJECT` *necessary*  
	The same as `<-yaml`
- `'(#:swap-when . COMPARE)`  
	The compare procedure for sorting.  
	This should be a lambda that accept 2 arguments.
	`mapping-ordered-yaml<-` will apply this lambda to each 2 yaml-mapping keys,
	if this lambda return true, then swap them.  
	If not set, `mapping-ordered-yaml<-` will sort by scheme function `string>?`  
	For example:
	- For a yaml content `{1: 2, 3: 4}`, `yaml<-` will parse it to `(list (lambda () '((1 . 2) (3 . 4))))`.  
	If `COMPARE` is `(lambda (l r) (< l r))`,
	means if the left key(here is `1`) is smaller than the right key(here is `3`),
	then swap them.  
	Hence `mapping-ordered-yaml<-` with this `#:swap-when` option will generate
	`(list '((3 . 4) (1 . 2)))`, swap the key-value pair.

**Note** that the result of `mapping-ordered-yaml<-` is
**different** from the original `SCHEME-YAML-OBJECT` .
Because it replace the procedure to list,
so you cannot distinguish yaml-mapping and yaml-list in it.

#### Check if a key is in yaml-mapping

```
(in-yaml-mapping? MAPPING KEY)
```

`in-yaml-mapping?` will return `#t` if `KEY` is in `MAPPING`.
As definition in *Read yaml file or string* section,
`MAPPING` should be a procedure that generate a "association list".

`in-yaml-mapping?` will compare by `equal?`

```
(in-yaml-mapping?? MAPPING KEY)
(in-yaml-mapping??? MAPPING KEY)
```

Similar to `in-yaml-mapping?`,
but `in-yaml-mapping??` will compare by `epv?`,
`in-yaml-mapping???` will compare by `ep?`.

## Examples

```lisp
(import libyaml)

(set! yaml (yaml<- `(#:input . "{c: d, a: b}")))
(map print (list
yaml ; will be a list that only contain 1 procedure (#<procedure>)
(mapping-ordered-yaml<- yaml) ; --> (((a . b) (c . d)))
(procedure? yaml) ; --> #f the top-level is always a list of yaml-document
(list? (car yaml)) ; --> #f
(procedure? (car yaml)) ; --> #t this way to check if it is a yaml-mapping
; (in-yaml-mapping? yaml "a") ; --> ERROR because the top level is always a list but not mapping
(in-yaml-mapping? (car yaml) "a") ; --> #t
(in-yaml-mapping? (car yaml) "x") ; --> #f
))

(set! yaml (yaml<- `(#:input . "[1, 2, -.inf, string]")))
(map print (list
yaml ; --> ((1 2 -inf.0 "string"))
(list? (car yaml)) ; --> #t
(map number? (car yaml)) ; --> (#t #t #t #f)
))
```

---

Content of `tmp.yaml`:
```yaml
- replace me: This will be changed
- a internal mapping:
    replace me: This will be changed
- replace me
- - replace me
- ignored
- "3.32" # string , also ignored
- 3.32 # number, also ignored
```

Content of `tmp.scm`:
> Replace all `replace me` to `HERE` when it is a element of list,
> or replace the value of it if the mapping key is `replace me`
```lisp
(import libyaml)

(define (replace-yaml-document yaml)
	(cond
		((list? yaml) (map replace-yaml-document yaml))
		((procedure? yaml)
			(let ((mapping (yaml)))
				(lambda ()  ; keep making mapping a procedure to distinguish mapping and list in the result
					(map
						(lambda (pair)
							(let*
								(
									(key
										(cond
											((procedure? (car pair)) (replace-yaml-document (car pair))) ; the key of mapping is a mapping
											((list? (car pair)) (replace-yaml-document (car pair))) ; the key of mapping is a list
											(else (car pair))
										))
									(value
										(if (string? key)
											(if (string=? key "replace me")
												"HERE"
												(replace-yaml-document (cdr pair)))
											(replace-yaml-document (cdr pair))))
								)
								(cons key value)
							))
						mapping
					))))
		(else
			(if (string? yaml)
				(if (string=? yaml "replace me") "HERE" yaml)
				yaml))
	)
)

(let ((yaml (yaml<- `(#:input . ,(open-input-file "tmp.yaml")))))
	(print (map replace-yaml-document yaml)) ; mapping will not print
	(print (mapping-ordered-yaml<- (map replace-yaml-document yaml))) ; mapping will be print
	(print (make-string 32 #\#))
	(<-yaml (map replace-yaml-document yaml)) ; print to stdout, generally screen
)
```

Output:
```
((#<procedure (?)> #<procedure (?)> HERE (HERE) ignored 3.32 3.32))                                                                                                                            ((((replace me . HERE)) ((a internal mapping (replace me . HERE))) HERE (HERE) ignored 3.32 3.32))
################################
---
- replace me: HERE
- a internal mapping:
    replace me: HERE
- HERE
- - HERE
- ignored
- '3.32'
- 3.32
...
```

## License

MIT

## Version History
