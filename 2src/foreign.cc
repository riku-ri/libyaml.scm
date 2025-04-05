#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <clang-c/Index.h>

#include <string>
#include <iostream>

#define MAX_strlen 0x100

extern enum CXChildVisitResult definitions
(
	CXCursor cursor ,
	CXCursor parent ,
	CXClientData client_data
);

template<typename _t> class raii_t {};

template<>
class raii_t<CXString> : public CXString
{
	public:
		raii_t<CXString>(const CXString & cxstring) : CXString(cxstring) {}
		~raii_t<CXString>() {clang_disposeString((CXString)(*this));}
};

const char * header[] = {
"(import (chicken foreign))" ,
"(foreign-declare \"#include <yaml.h>\")"
};

int
main(int argc , char * argv[])
{
	if(argc <= 0) exit(0);
	CXIndex clang_index = clang_createIndex(0 , 0);
	for(size_t i=0 ; i<sizeof(header)/sizeof(*header) ; i++) printf("%s\n" , header[i]);
	const char * args[] = {"-I."};
	CXTranslationUnit clang_tran_unit = clang_parseTranslationUnit
	(
		clang_index ,
		argv[1] ,
		args ,
		sizeof(args)/sizeof(*args) ,
		NULL ,
		0 ,
		CXTranslationUnit_None
	);
	clang_visitChildren
	(
		clang_getTranslationUnitCursor(clang_tran_unit) ,
		definitions ,
		NULL
	);
	return 0;
}

static int _is(enum CXCursorKind cxcursorkind , CXType cxtype)
{
	enum CXCursorKind typekind = clang_getCursorKind(clang_getTypeDeclaration(cxtype));
	CXType truetype = clang_getTypedefDeclUnderlyingType(
		clang_getTypeDeclaration(cxtype)
	);
	enum CXCursorKind truetypekind = clang_getCursorKind(
		clang_getTypeDeclaration(truetype)
	);
	if(typekind==cxcursorkind || truetypekind==cxcursorkind)
	{
		return (!0);
	}
	return 0;
}

enum CXChildVisitResult
definitions
(
	CXCursor cursor ,
	CXCursor parent ,
	void * client_data
)
{
	raii_t<CXString> cxstring = clang_getCursorSpelling(cursor);
	//printf("%d\n" , clang_getCursorKind(cursor));
	CXSourceLocation location = clang_getCursorLocation(cursor);
	CXFile file;
	unsigned line , column , offset;
	clang_getSpellingLocation(location , &file , &line , &column , &offset);
	raii_t<CXString> fname = clang_getFileName(file);
	if(!file)
	{
		return CXChildVisit_Continue;
	}
	const char * fname_p = clang_getCString(fname);
	while(*++fname_p);
	for(int i=0 ; i<sizeof("yaml.h") ; i++) fname_p--;
	if(strncmp(fname_p , "/yaml.h" , sizeof("yaml.h"))!=0)
	/* the prefix '/' is required to distinguish from strings like libyaml.h */
	/* use './yaml.h' if without path */
	{
		return CXChildVisit_Continue;
	}

	if(0);
	else if(clang_getCursorKind(cursor)==CXCursor_TypedefDecl)
	{
		/* XXX enum may be anonymous, export enum type string is not always valid */
		//CXType cxtype = clang_getTypedefDeclUnderlyingType(cursor);
		//enum CXCursorKind typekind = clang_getCursorKind(clang_getTypeDeclaration(cxtype));
		//raii_t<CXString> cxtype = clang_getTypeSpelling(cxtype);
		//if(typekind==CXCursor_EnumDecl)
		//{
		//	//printf("(define-foreign-type %s int)\n" , clang_getCString(cxstring));
		//}
	}
	else if(clang_getCursorKind(cursor)==CXCursor_FunctionDecl)
	{
		CXType type = clang_getCursorResultType(cursor);
		CXType endpoint = clang_getTypedefDeclUnderlyingType(
			clang_getTypeDeclaration(type)
		);
		CXType truetype = endpoint.kind==CXType_Invalid ? type : endpoint;
		raii_t<CXString> cxtruetype = clang_getTypeSpelling(truetype);
		if(_is(CXCursor_StructDecl , type) || _is(CXCursor_UnionDecl , type))
		{
			raii_t<CXString> cxtype = clang_getTypeSpelling(type);
			fprintf(stderr , "[ERROR] struct/union type [%s] is not supported\n" ,
				clang_getCString(cxtype)
			);
			abort();
		}
		const char * typestring = clang_getCString(cxtruetype);
		raii_t<CXString> maybe_size_t = clang_getTypeSpelling(type);
		if(strcmp("size_t" , clang_getCString(maybe_size_t))==0) typestring = "size_t";
		if(truetype.kind==CXType_Pointer)
		{
			CXType endpoint = clang_getTypedefDeclUnderlyingType(
				clang_getTypeDeclaration(clang_getPointeeType(truetype))
			);
			CXType truepoint = endpoint.kind==CXType_Invalid ? clang_getPointeeType(truetype) : endpoint;
			if(0);
			else if(truepoint.kind==CXType_Char_U) typestring = "c-string";
			else if(truepoint.kind==CXType_UChar) typestring = "c-string";
			else if(truepoint.kind==CXType_Char_S) typestring = "c-string";
			else if(truepoint.kind==CXType_SChar) typestring = "c-string";
			else if(truepoint.kind==CXType_WChar) typestring = "c-string";
			else typestring = "c-pointer";
		}
		else if(_is(CXCursor_EnumDecl , type)) typestring = "int";
		char * replace_whitespace = (char*)malloc(strnlen(typestring , MAX_strlen) + 1);
		memset(replace_whitespace , 0 , strnlen(typestring , MAX_strlen) + 1);
		strncpy(replace_whitespace , typestring , strnlen(typestring , MAX_strlen));
		char * r = replace_whitespace; r--;
		while(++*r) if(*r==' ') *r = '-' ;
		printf("(define %s (foreign-lambda %s \"%s\"" ,
			clang_getCString(cxstring) ,
			replace_whitespace ,
			clang_getCString(cxstring)
		);
		free(replace_whitespace);

		for(int i=0 ; i<clang_Cursor_getNumArguments(cursor) ; i++)
		{
			CXCursor _cursor = clang_Cursor_getArgument(cursor , i);
			CXType type = clang_getCursorType(_cursor);
			CXType endpoint = clang_getTypedefDeclUnderlyingType(
				clang_getTypeDeclaration(type)
			);
			CXType truetype = endpoint.kind==CXType_Invalid ? type : endpoint;
			raii_t<CXString> cxtruetype = clang_getTypeSpelling(truetype);
			if(_is(CXCursor_StructDecl , type) || _is(CXCursor_UnionDecl , type))
			{
				raii_t<CXString> cxtype = clang_getTypeSpelling(type);
				fprintf(stderr , "[ERROR] struct/union type [%s] is not supported\n" ,
					clang_getCString(cxtype)
				);
				abort();
			}
			const char * typestring = clang_getCString(cxtruetype);
			raii_t<CXString> maybe_size_t = clang_getTypeSpelling(type);
			if(strcmp("size_t" , clang_getCString(maybe_size_t))==0) typestring = "size_t";
			if(truetype.kind==CXType_Pointer)
			{
				CXType endpoint = clang_getTypedefDeclUnderlyingType(
					clang_getTypeDeclaration(clang_getPointeeType(truetype))
				);
				CXType truepoint = endpoint.kind==CXType_Invalid ? clang_getPointeeType(truetype) : endpoint;
				if(0);
				else if(truepoint.kind==CXType_Char_U) typestring = "c-string";
				else if(truepoint.kind==CXType_UChar) typestring = "c-string";
				else if(truepoint.kind==CXType_Char_S) typestring = "c-string";
				else if(truepoint.kind==CXType_SChar) typestring = "c-string";
				else if(truepoint.kind==CXType_WChar) typestring = "c-string";
				else typestring = "c-pointer";
			}
			else if(_is(CXCursor_EnumDecl , type)) typestring = "int";
			char * replace_whitespace = (char*)malloc(strnlen(typestring , MAX_strlen) + 1);
			memset(replace_whitespace , 0 , strnlen(typestring , MAX_strlen) + 1);
			strncpy(replace_whitespace , typestring , strnlen(typestring , MAX_strlen));
			char * r = replace_whitespace; r--;
			while(++*r) if(*r==' ') *r = '-' ;
			printf("\n\t%s" , replace_whitespace);
			free(replace_whitespace);
		}
		printf("))\n");
		return CXChildVisit_Continue;
	}
	else if(clang_getCursorKind(cursor)==CXCursor_EnumDecl)
	{
		/* XXX enum may be anonymous, export enum type string is not always valid */
		//printf(
		//	"(define-foreign-type %s int)\n"
		//	"(define >%s< (list))\n" ,
		//	clang_getCString(cxstring) ,
		//	clang_getCString(cxstring)
		//);
		//char * enum_string = strdup(clang_getCString(cxstring));
		//enum_string[strlen(enum_string)-1] = 't';
		//printf("(define-foreign-type %s int)\n" , enum_string);
		//free(enum_string);
		return CXChildVisit_Recurse;
	}
	else if(clang_getCursorKind(cursor)==CXCursor_EnumConstantDecl)
	{
		raii_t<CXString> cxparent = clang_getCursorSpelling(parent);
		printf(
			"(define %s (foreign-value \"(%s)\" %s))\n"
			//"(define >%s< (cons (cons %s (quote %s)) >%s<))\n"
			,
			clang_getCString(cxstring) ,
			clang_getCString(cxstring) ,
			"int" //clang_getCString(cxparent)
			//,
			//clang_getCString(cxparent) ,
			//clang_getCString(cxstring) ,
			//clang_getCString(cxstring) ,
			//clang_getCString(cxparent)
		);
		return CXChildVisit_Continue;
	}
	return CXChildVisit_Continue;
}
