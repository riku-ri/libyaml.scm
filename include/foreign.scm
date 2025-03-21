(import (chicken foreign))
(foreign-declare "#include <yaml.h>")
(define yaml_get_version_string (foreign-lambda c-string "yaml_get_version_string"))
(define yaml_get_version (foreign-lambda void "yaml_get_version"
	c-pointer
	c-pointer
	c-pointer))
(define YAML_ANY_ENCODING (foreign-value "(YAML_ANY_ENCODING)" int))
(define YAML_UTF8_ENCODING (foreign-value "(YAML_UTF8_ENCODING)" int))
(define YAML_UTF16LE_ENCODING (foreign-value "(YAML_UTF16LE_ENCODING)" int))
(define YAML_UTF16BE_ENCODING (foreign-value "(YAML_UTF16BE_ENCODING)" int))
(define YAML_ANY_BREAK (foreign-value "(YAML_ANY_BREAK)" int))
(define YAML_CR_BREAK (foreign-value "(YAML_CR_BREAK)" int))
(define YAML_LN_BREAK (foreign-value "(YAML_LN_BREAK)" int))
(define YAML_CRLN_BREAK (foreign-value "(YAML_CRLN_BREAK)" int))
(define YAML_NO_ERROR (foreign-value "(YAML_NO_ERROR)" int))
(define YAML_MEMORY_ERROR (foreign-value "(YAML_MEMORY_ERROR)" int))
(define YAML_READER_ERROR (foreign-value "(YAML_READER_ERROR)" int))
(define YAML_SCANNER_ERROR (foreign-value "(YAML_SCANNER_ERROR)" int))
(define YAML_PARSER_ERROR (foreign-value "(YAML_PARSER_ERROR)" int))
(define YAML_COMPOSER_ERROR (foreign-value "(YAML_COMPOSER_ERROR)" int))
(define YAML_WRITER_ERROR (foreign-value "(YAML_WRITER_ERROR)" int))
(define YAML_EMITTER_ERROR (foreign-value "(YAML_EMITTER_ERROR)" int))
(define YAML_ANY_SCALAR_STYLE (foreign-value "(YAML_ANY_SCALAR_STYLE)" int))
(define YAML_PLAIN_SCALAR_STYLE (foreign-value "(YAML_PLAIN_SCALAR_STYLE)" int))
(define YAML_SINGLE_QUOTED_SCALAR_STYLE (foreign-value "(YAML_SINGLE_QUOTED_SCALAR_STYLE)" int))
(define YAML_DOUBLE_QUOTED_SCALAR_STYLE (foreign-value "(YAML_DOUBLE_QUOTED_SCALAR_STYLE)" int))
(define YAML_LITERAL_SCALAR_STYLE (foreign-value "(YAML_LITERAL_SCALAR_STYLE)" int))
(define YAML_FOLDED_SCALAR_STYLE (foreign-value "(YAML_FOLDED_SCALAR_STYLE)" int))
(define YAML_ANY_SEQUENCE_STYLE (foreign-value "(YAML_ANY_SEQUENCE_STYLE)" int))
(define YAML_BLOCK_SEQUENCE_STYLE (foreign-value "(YAML_BLOCK_SEQUENCE_STYLE)" int))
(define YAML_FLOW_SEQUENCE_STYLE (foreign-value "(YAML_FLOW_SEQUENCE_STYLE)" int))
(define YAML_ANY_MAPPING_STYLE (foreign-value "(YAML_ANY_MAPPING_STYLE)" int))
(define YAML_BLOCK_MAPPING_STYLE (foreign-value "(YAML_BLOCK_MAPPING_STYLE)" int))
(define YAML_FLOW_MAPPING_STYLE (foreign-value "(YAML_FLOW_MAPPING_STYLE)" int))
(define YAML_NO_TOKEN (foreign-value "(YAML_NO_TOKEN)" int))
(define YAML_STREAM_START_TOKEN (foreign-value "(YAML_STREAM_START_TOKEN)" int))
(define YAML_STREAM_END_TOKEN (foreign-value "(YAML_STREAM_END_TOKEN)" int))
(define YAML_VERSION_DIRECTIVE_TOKEN (foreign-value "(YAML_VERSION_DIRECTIVE_TOKEN)" int))
(define YAML_TAG_DIRECTIVE_TOKEN (foreign-value "(YAML_TAG_DIRECTIVE_TOKEN)" int))
(define YAML_DOCUMENT_START_TOKEN (foreign-value "(YAML_DOCUMENT_START_TOKEN)" int))
(define YAML_DOCUMENT_END_TOKEN (foreign-value "(YAML_DOCUMENT_END_TOKEN)" int))
(define YAML_BLOCK_SEQUENCE_START_TOKEN (foreign-value "(YAML_BLOCK_SEQUENCE_START_TOKEN)" int))
(define YAML_BLOCK_MAPPING_START_TOKEN (foreign-value "(YAML_BLOCK_MAPPING_START_TOKEN)" int))
(define YAML_BLOCK_END_TOKEN (foreign-value "(YAML_BLOCK_END_TOKEN)" int))
(define YAML_FLOW_SEQUENCE_START_TOKEN (foreign-value "(YAML_FLOW_SEQUENCE_START_TOKEN)" int))
(define YAML_FLOW_SEQUENCE_END_TOKEN (foreign-value "(YAML_FLOW_SEQUENCE_END_TOKEN)" int))
(define YAML_FLOW_MAPPING_START_TOKEN (foreign-value "(YAML_FLOW_MAPPING_START_TOKEN)" int))
(define YAML_FLOW_MAPPING_END_TOKEN (foreign-value "(YAML_FLOW_MAPPING_END_TOKEN)" int))
(define YAML_BLOCK_ENTRY_TOKEN (foreign-value "(YAML_BLOCK_ENTRY_TOKEN)" int))
(define YAML_FLOW_ENTRY_TOKEN (foreign-value "(YAML_FLOW_ENTRY_TOKEN)" int))
(define YAML_KEY_TOKEN (foreign-value "(YAML_KEY_TOKEN)" int))
(define YAML_VALUE_TOKEN (foreign-value "(YAML_VALUE_TOKEN)" int))
(define YAML_ALIAS_TOKEN (foreign-value "(YAML_ALIAS_TOKEN)" int))
(define YAML_ANCHOR_TOKEN (foreign-value "(YAML_ANCHOR_TOKEN)" int))
(define YAML_TAG_TOKEN (foreign-value "(YAML_TAG_TOKEN)" int))
(define YAML_SCALAR_TOKEN (foreign-value "(YAML_SCALAR_TOKEN)" int))
(define yaml_token_delete (foreign-lambda void "yaml_token_delete"
	c-pointer))
(define YAML_NO_EVENT (foreign-value "(YAML_NO_EVENT)" int))
(define YAML_STREAM_START_EVENT (foreign-value "(YAML_STREAM_START_EVENT)" int))
(define YAML_STREAM_END_EVENT (foreign-value "(YAML_STREAM_END_EVENT)" int))
(define YAML_DOCUMENT_START_EVENT (foreign-value "(YAML_DOCUMENT_START_EVENT)" int))
(define YAML_DOCUMENT_END_EVENT (foreign-value "(YAML_DOCUMENT_END_EVENT)" int))
(define YAML_ALIAS_EVENT (foreign-value "(YAML_ALIAS_EVENT)" int))
(define YAML_SCALAR_EVENT (foreign-value "(YAML_SCALAR_EVENT)" int))
(define YAML_SEQUENCE_START_EVENT (foreign-value "(YAML_SEQUENCE_START_EVENT)" int))
(define YAML_SEQUENCE_END_EVENT (foreign-value "(YAML_SEQUENCE_END_EVENT)" int))
(define YAML_MAPPING_START_EVENT (foreign-value "(YAML_MAPPING_START_EVENT)" int))
(define YAML_MAPPING_END_EVENT (foreign-value "(YAML_MAPPING_END_EVENT)" int))
(define yaml_stream_start_event_initialize (foreign-lambda int "yaml_stream_start_event_initialize"
	c-pointer
	int))
(define yaml_stream_end_event_initialize (foreign-lambda int "yaml_stream_end_event_initialize"
	c-pointer))
(define yaml_document_start_event_initialize (foreign-lambda int "yaml_document_start_event_initialize"
	c-pointer
	c-pointer
	c-pointer
	c-pointer
	int))
(define yaml_document_end_event_initialize (foreign-lambda int "yaml_document_end_event_initialize"
	c-pointer
	int))
(define yaml_alias_event_initialize (foreign-lambda int "yaml_alias_event_initialize"
	c-pointer
	c-string))
(define yaml_scalar_event_initialize (foreign-lambda int "yaml_scalar_event_initialize"
	c-pointer
	c-string
	c-string
	c-string
	int
	int
	int
	int))
(define yaml_sequence_start_event_initialize (foreign-lambda int "yaml_sequence_start_event_initialize"
	c-pointer
	c-string
	c-string
	int
	int))
(define yaml_sequence_end_event_initialize (foreign-lambda int "yaml_sequence_end_event_initialize"
	c-pointer))
(define yaml_mapping_start_event_initialize (foreign-lambda int "yaml_mapping_start_event_initialize"
	c-pointer
	c-string
	c-string
	int
	int))
(define yaml_mapping_end_event_initialize (foreign-lambda int "yaml_mapping_end_event_initialize"
	c-pointer))
(define yaml_event_delete (foreign-lambda void "yaml_event_delete"
	c-pointer))
(define YAML_NO_NODE (foreign-value "(YAML_NO_NODE)" int))
(define YAML_SCALAR_NODE (foreign-value "(YAML_SCALAR_NODE)" int))
(define YAML_SEQUENCE_NODE (foreign-value "(YAML_SEQUENCE_NODE)" int))
(define YAML_MAPPING_NODE (foreign-value "(YAML_MAPPING_NODE)" int))
(define yaml_document_initialize (foreign-lambda int "yaml_document_initialize"
	c-pointer
	c-pointer
	c-pointer
	c-pointer
	int
	int))
(define yaml_document_delete (foreign-lambda void "yaml_document_delete"
	c-pointer))
(define yaml_document_get_node (foreign-lambda c-pointer "yaml_document_get_node"
	c-pointer
	int))
(define yaml_document_get_root_node (foreign-lambda c-pointer "yaml_document_get_root_node"
	c-pointer))
(define yaml_document_add_scalar (foreign-lambda int "yaml_document_add_scalar"
	c-pointer
	c-string
	c-string
	int
	int))
(define yaml_document_add_sequence (foreign-lambda int "yaml_document_add_sequence"
	c-pointer
	c-string
	int))
(define yaml_document_add_mapping (foreign-lambda int "yaml_document_add_mapping"
	c-pointer
	c-string
	int))
(define yaml_document_append_sequence_item (foreign-lambda int "yaml_document_append_sequence_item"
	c-pointer
	int
	int))
(define yaml_document_append_mapping_pair (foreign-lambda int "yaml_document_append_mapping_pair"
	c-pointer
	int
	int
	int))
(define YAML_PARSE_STREAM_START_STATE (foreign-value "(YAML_PARSE_STREAM_START_STATE)" int))
(define YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE (foreign-value "(YAML_PARSE_IMPLICIT_DOCUMENT_START_STATE)" int))
(define YAML_PARSE_DOCUMENT_START_STATE (foreign-value "(YAML_PARSE_DOCUMENT_START_STATE)" int))
(define YAML_PARSE_DOCUMENT_CONTENT_STATE (foreign-value "(YAML_PARSE_DOCUMENT_CONTENT_STATE)" int))
(define YAML_PARSE_DOCUMENT_END_STATE (foreign-value "(YAML_PARSE_DOCUMENT_END_STATE)" int))
(define YAML_PARSE_BLOCK_NODE_STATE (foreign-value "(YAML_PARSE_BLOCK_NODE_STATE)" int))
(define YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE (foreign-value "(YAML_PARSE_BLOCK_NODE_OR_INDENTLESS_SEQUENCE_STATE)" int))
(define YAML_PARSE_FLOW_NODE_STATE (foreign-value "(YAML_PARSE_FLOW_NODE_STATE)" int))
(define YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE (foreign-value "(YAML_PARSE_BLOCK_SEQUENCE_FIRST_ENTRY_STATE)" int))
(define YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE (foreign-value "(YAML_PARSE_BLOCK_SEQUENCE_ENTRY_STATE)" int))
(define YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE (foreign-value "(YAML_PARSE_INDENTLESS_SEQUENCE_ENTRY_STATE)" int))
(define YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE (foreign-value "(YAML_PARSE_BLOCK_MAPPING_FIRST_KEY_STATE)" int))
(define YAML_PARSE_BLOCK_MAPPING_KEY_STATE (foreign-value "(YAML_PARSE_BLOCK_MAPPING_KEY_STATE)" int))
(define YAML_PARSE_BLOCK_MAPPING_VALUE_STATE (foreign-value "(YAML_PARSE_BLOCK_MAPPING_VALUE_STATE)" int))
(define YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE (foreign-value "(YAML_PARSE_FLOW_SEQUENCE_FIRST_ENTRY_STATE)" int))
(define YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE (foreign-value "(YAML_PARSE_FLOW_SEQUENCE_ENTRY_STATE)" int))
(define YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE (foreign-value "(YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_KEY_STATE)" int))
(define YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE (foreign-value "(YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_VALUE_STATE)" int))
(define YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE (foreign-value "(YAML_PARSE_FLOW_SEQUENCE_ENTRY_MAPPING_END_STATE)" int))
(define YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE (foreign-value "(YAML_PARSE_FLOW_MAPPING_FIRST_KEY_STATE)" int))
(define YAML_PARSE_FLOW_MAPPING_KEY_STATE (foreign-value "(YAML_PARSE_FLOW_MAPPING_KEY_STATE)" int))
(define YAML_PARSE_FLOW_MAPPING_VALUE_STATE (foreign-value "(YAML_PARSE_FLOW_MAPPING_VALUE_STATE)" int))
(define YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE (foreign-value "(YAML_PARSE_FLOW_MAPPING_EMPTY_VALUE_STATE)" int))
(define YAML_PARSE_END_STATE (foreign-value "(YAML_PARSE_END_STATE)" int))
(define yaml_parser_initialize (foreign-lambda int "yaml_parser_initialize"
	c-pointer))
(define yaml_parser_delete (foreign-lambda void "yaml_parser_delete"
	c-pointer))
(define yaml_parser_set_input_string (foreign-lambda void "yaml_parser_set_input_string"
	c-pointer
	c-string
	size_t))
(define yaml_parser_set_input_file (foreign-lambda void "yaml_parser_set_input_file"
	c-pointer
	c-pointer))
(define yaml_parser_set_input (foreign-lambda void "yaml_parser_set_input"
	c-pointer
	c-pointer
	c-pointer))
(define yaml_parser_set_encoding (foreign-lambda void "yaml_parser_set_encoding"
	c-pointer
	int))
(define yaml_parser_scan (foreign-lambda int "yaml_parser_scan"
	c-pointer
	c-pointer))
(define yaml_parser_parse (foreign-lambda int "yaml_parser_parse"
	c-pointer
	c-pointer))
(define yaml_parser_load (foreign-lambda int "yaml_parser_load"
	c-pointer
	c-pointer))
(define yaml_set_max_nest_level (foreign-lambda void "yaml_set_max_nest_level"
	int))
(define YAML_EMIT_STREAM_START_STATE (foreign-value "(YAML_EMIT_STREAM_START_STATE)" int))
(define YAML_EMIT_FIRST_DOCUMENT_START_STATE (foreign-value "(YAML_EMIT_FIRST_DOCUMENT_START_STATE)" int))
(define YAML_EMIT_DOCUMENT_START_STATE (foreign-value "(YAML_EMIT_DOCUMENT_START_STATE)" int))
(define YAML_EMIT_DOCUMENT_CONTENT_STATE (foreign-value "(YAML_EMIT_DOCUMENT_CONTENT_STATE)" int))
(define YAML_EMIT_DOCUMENT_END_STATE (foreign-value "(YAML_EMIT_DOCUMENT_END_STATE)" int))
(define YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE (foreign-value "(YAML_EMIT_FLOW_SEQUENCE_FIRST_ITEM_STATE)" int))
(define YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE (foreign-value "(YAML_EMIT_FLOW_SEQUENCE_ITEM_STATE)" int))
(define YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE (foreign-value "(YAML_EMIT_FLOW_MAPPING_FIRST_KEY_STATE)" int))
(define YAML_EMIT_FLOW_MAPPING_KEY_STATE (foreign-value "(YAML_EMIT_FLOW_MAPPING_KEY_STATE)" int))
(define YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE (foreign-value "(YAML_EMIT_FLOW_MAPPING_SIMPLE_VALUE_STATE)" int))
(define YAML_EMIT_FLOW_MAPPING_VALUE_STATE (foreign-value "(YAML_EMIT_FLOW_MAPPING_VALUE_STATE)" int))
(define YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE (foreign-value "(YAML_EMIT_BLOCK_SEQUENCE_FIRST_ITEM_STATE)" int))
(define YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE (foreign-value "(YAML_EMIT_BLOCK_SEQUENCE_ITEM_STATE)" int))
(define YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE (foreign-value "(YAML_EMIT_BLOCK_MAPPING_FIRST_KEY_STATE)" int))
(define YAML_EMIT_BLOCK_MAPPING_KEY_STATE (foreign-value "(YAML_EMIT_BLOCK_MAPPING_KEY_STATE)" int))
(define YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE (foreign-value "(YAML_EMIT_BLOCK_MAPPING_SIMPLE_VALUE_STATE)" int))
(define YAML_EMIT_BLOCK_MAPPING_VALUE_STATE (foreign-value "(YAML_EMIT_BLOCK_MAPPING_VALUE_STATE)" int))
(define YAML_EMIT_END_STATE (foreign-value "(YAML_EMIT_END_STATE)" int))
(define yaml_emitter_initialize (foreign-lambda int "yaml_emitter_initialize"
	c-pointer))
(define yaml_emitter_delete (foreign-lambda void "yaml_emitter_delete"
	c-pointer))
(define yaml_emitter_set_output_string (foreign-lambda void "yaml_emitter_set_output_string"
	c-pointer
	c-string
	size_t
	c-pointer))
(define yaml_emitter_set_output_file (foreign-lambda void "yaml_emitter_set_output_file"
	c-pointer
	c-pointer))
(define yaml_emitter_set_output (foreign-lambda void "yaml_emitter_set_output"
	c-pointer
	c-pointer
	c-pointer))
(define yaml_emitter_set_encoding (foreign-lambda void "yaml_emitter_set_encoding"
	c-pointer
	int))
(define yaml_emitter_set_canonical (foreign-lambda void "yaml_emitter_set_canonical"
	c-pointer
	int))
(define yaml_emitter_set_indent (foreign-lambda void "yaml_emitter_set_indent"
	c-pointer
	int))
(define yaml_emitter_set_width (foreign-lambda void "yaml_emitter_set_width"
	c-pointer
	int))
(define yaml_emitter_set_unicode (foreign-lambda void "yaml_emitter_set_unicode"
	c-pointer
	int))
(define yaml_emitter_set_break (foreign-lambda void "yaml_emitter_set_break"
	c-pointer
	int))
(define yaml_emitter_emit (foreign-lambda int "yaml_emitter_emit"
	c-pointer
	c-pointer))
(define yaml_emitter_open (foreign-lambda int "yaml_emitter_open"
	c-pointer))
(define yaml_emitter_close (foreign-lambda int "yaml_emitter_close"
	c-pointer))
(define yaml_emitter_dump (foreign-lambda int "yaml_emitter_dump"
	c-pointer
	c-pointer))
(define yaml_emitter_flush (foreign-lambda int "yaml_emitter_flush"
	c-pointer))
