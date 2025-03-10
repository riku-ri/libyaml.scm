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

static int _is_enum(CXType cxtype)
{
	enum CXCursorKind typekind = clang_getCursorKind(clang_getTypeDeclaration(cxtype));
	CXType truetype = clang_getTypedefDeclUnderlyingType(
		clang_getTypeDeclaration(cxtype)
	);
	enum CXCursorKind truetypekind = clang_getCursorKind(
		clang_getTypeDeclaration(truetype)
	);
	if(typekind==CXCursor_EnumDecl || truetypekind==CXCursor_EnumDecl)
	{
		return (!0);
	}
	return 0;
}

static int _is_struct(CXType cxtype)
{
	enum CXCursorKind typekind = clang_getCursorKind(clang_getTypeDeclaration(cxtype));
	CXType truetype = clang_getTypedefDeclUnderlyingType(
		clang_getTypeDeclaration(cxtype)
	);
	enum CXCursorKind truetypekind = clang_getCursorKind(
		clang_getTypeDeclaration(truetype)
	);
	if(typekind==CXCursor_StructDecl || truetypekind==CXCursor_StructDecl)
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
	CXString cxstring = clang_getCursorSpelling(cursor);
	//printf("%d\n" , clang_getCursorKind(cursor));
	CXSourceLocation location = clang_getCursorLocation(cursor);
	CXFile file;
	unsigned line , column , offset;
	clang_getSpellingLocation(location , &file , &line , &column , &offset);
	CXString fname = clang_getFileName(file);
	if(!file)
	{
		clang_disposeString(fname);
		clang_disposeString(cxstring);
		return CXChildVisit_Continue;
	}
	const char * fname_p = clang_getCString(fname);
	while(*++fname_p);
	for(int i=0 ; i<sizeof("yaml.h") ; i++) fname_p--;
	if(strncmp(fname_p , "/yaml.h" , sizeof("yaml.h"))!=0)
	/* the prefix '/' is required to distinguish from strings like libyaml.h */
	/* use './yaml.h' if without path */
	{
		clang_disposeString(fname);
		clang_disposeString(cxstring);
		return CXChildVisit_Continue;
	}
	clang_disposeString(fname);

	if(0);
	else if(clang_getCursorKind(cursor)==CXCursor_TypedefDecl)
	{
		/* XXX enum may be anonymous, export enum type string is not always valid */
		//CXType cxtype = clang_getTypedefDeclUnderlyingType(cursor);
		//enum CXCursorKind typekind = clang_getCursorKind(clang_getTypeDeclaration(cxtype));
		//CXString cxtype = clang_getTypeSpelling(cxtype);
		//clang_disposeString(cxtype);
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
		CXType * truetype = endpoint.kind==CXType_Invalid ? &type : &endpoint;
		CXString cxtruetype = clang_getTypeSpelling(*truetype);
		if(_is_struct(type))
		{
			CXString cxtype = clang_getTypeSpelling(type);
			fprintf(stderr , "[ERROR] struct type [%s] is not supported\n" ,
				clang_getCString(cxtype)
			);
			clang_disposeString(cxtype);
			clang_disposeString(cxstring);
			abort();
		}
		const char * typestring = clang_getCString(cxtruetype);
		if(truetype->kind==CXType_Pointer)
		{
			if(0);
			else if(clang_getPointeeType(*truetype).kind==CXType_Char_U) typestring = "c-string";
			else if(clang_getPointeeType(*truetype).kind==CXType_UChar) typestring = "c-string";
			else if(clang_getPointeeType(*truetype).kind==CXType_Char_S) typestring = "c-string";
			else if(clang_getPointeeType(*truetype).kind==CXType_SChar) typestring = "c-string";
			else if(clang_getPointeeType(*truetype).kind==CXType_WChar) typestring = "c-string";
			else typestring = "c-pointer";
		}
		else if(_is_enum(type)) typestring = "int";
		printf("(define %s (foreign-lambda %s \"%s\"" ,
			clang_getCString(cxstring) ,
			typestring ,
			clang_getCString(cxstring)
		);
		clang_disposeString(cxtruetype);
		for(int i=0 ; i<clang_Cursor_getNumArguments(cursor) ; i++)
		{
			CXCursor _cursor = clang_Cursor_getArgument(cursor , i);
			CXType type = clang_getCursorType(_cursor);
			CXType endpoint = clang_getTypedefDeclUnderlyingType(
				clang_getTypeDeclaration(type)
			);
			CXType * truetype = endpoint.kind==CXType_Invalid ? &type : &endpoint;
			CXString cxtruetype = clang_getTypeSpelling(*truetype);
			if(_is_struct(type))
			{
				CXString cxtype = clang_getTypeSpelling(type);
				fprintf(stderr , "[ERROR] struct type [%s] is not supported\n" ,
					clang_getCString(cxtype)
				);
				clang_disposeString(cxtype);
				clang_disposeString(cxstring);
				abort();
			}
			const char * typestring = clang_getCString(cxtruetype);
			if(truetype->kind==CXType_Pointer)
			{
				if(0);
				else if(clang_getPointeeType(*truetype).kind==CXType_Char_U) typestring = "c-string";
				else if(clang_getPointeeType(*truetype).kind==CXType_UChar) typestring = "c-string";
				else if(clang_getPointeeType(*truetype).kind==CXType_Char_S) typestring = "c-string";
				else if(clang_getPointeeType(*truetype).kind==CXType_SChar) typestring = "c-string";
				else if(clang_getPointeeType(*truetype).kind==CXType_WChar) typestring = "c-string";
				else typestring = "c-pointer";
			}
			else if(_is_enum(type)) typestring = "int";
			printf("\n\t%s" , typestring);
			clang_disposeString(cxtruetype);
		}
		printf("))\n");
		clang_disposeString(cxstring);
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
		//clang_disposeString(cxstring);
		return CXChildVisit_Recurse;
	}
	else if(clang_getCursorKind(cursor)==CXCursor_EnumConstantDecl)
	{
		CXString cxparent = clang_getCursorSpelling(parent);
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
		clang_disposeString(cxstring);
		clang_disposeString(cxparent);
		return CXChildVisit_Continue;
	}
	clang_disposeString(cxstring);
	return CXChildVisit_Continue;
}
