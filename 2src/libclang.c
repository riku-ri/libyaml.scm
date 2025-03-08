#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <clang-c/Index.h>

extern enum CXChildVisitResult definitions
(
	CXCursor cursor ,
	CXCursor parent ,
	CXClientData client_data
);

char * header[] = {
"(import (chicken foreign))" ,
"(foreign-declare \"#include <yaml.h>\")"
};

int
main(int argc , char * argv[])
{
	if(argc <= 0) exit(0);
	CXIndex clang_index = clang_createIndex(0 , 0);
	for(size_t i=0 ; i<sizeof(header)/sizeof(*header) ; i++) printf("%s\n" , header[i]);
	CXTranslationUnit clang_tran_unit = clang_parseTranslationUnit
	(
		clang_index ,
		argv[1] ,
		NULL ,
		0 ,
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

enum CXChildVisitResult
definitions
(
	CXCursor cursor ,
	CXCursor parent ,
	void * client_data
)
{
	CXString cxstring = clang_getCursorSpelling(cursor);
	CXString cxkind = clang_getCursorKindSpelling(clang_getCursorKind(cursor));
	printf("%s\n" , clang_getCString(cxkind));
	clang_disposeString(cxkind);
	if(0);
	else if(clang_getCursorKind(cursor)==CXCursor_TypedefDecl)
	{
		CXType type = clang_getTypedefDeclUnderlyingType(cursor);
		CXString cxtype = clang_getTypeSpelling(type);
		printf("typedef %s %s\n" , clang_getCString(cxtype) , clang_getCString(cxstring));
		clang_disposeString(cxtype);
	}
	else if(clang_getCursorKind(cursor)==CXCursor_FunctionDecl)
	{
		//printf("%s\n" , clang_getCString(cxstring));
		clang_disposeString(cxstring);
		return CXChildVisit_Recurse;
	}
	else if(clang_getCursorKind(cursor)==CXCursor_EnumDecl)
	{
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
		//clang_disposeString(cxstring);
		//return CXChildVisit_Recurse;
	}
	else if(clang_getCursorKind(cursor)==CXCursor_EnumConstantDecl)
	{
		//CXString cxparent = clang_getCursorSpelling(parent);
		//printf(
		//	"(define %s (foreign-value \"%s\" %s))\n"
		//	"(define >%s< (cons (cons %s (quote %s)) >%s<))\n"
		//	,
		//	clang_getCString(cxstring) ,
		//	clang_getCString(cxstring) ,
		//	clang_getCString(cxparent)
		//	,
		//	clang_getCString(cxparent) ,
		//	clang_getCString(cxstring) ,
		//	clang_getCString(cxstring) ,
		//	clang_getCString(cxparent)
		//);
		//clang_disposeString(cxstring);
		//clang_disposeString(cxparent);
		//return CXChildVisit_Continue;
	}
	else return CXChildVisit_Continue;
	clang_disposeString(cxstring);
	return CXChildVisit_Continue;
}
