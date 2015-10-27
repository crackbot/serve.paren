# Manual for serving Parenscript

###### \[in package SERVE.PAREN\]
## serve.paren ASDF System Details

- Version: 0.0.1
- Description: Simple way to define and serve parenscript libs in webapps.
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>
- Maintainer: Crackbot <thecrackbot@gmail.com>

serve.paren package lets you define parenscript libraries, and
either serve them using one of web server hookups or produce an
optimized and compressed version of resulting javascript code

## How to define a library

There are two general ways of how to define a parenscript library,
which one to use mostly depends on your taste and usecase.

- [function] DEFPSLIB LIBNAME &KEY SYSTEM-SOURCE-DIR PAREN-SOURCE-DIR RUNTIME FILES PACKAGE (CUSTOM-FILES-ORDER #'IDENTITY) (FILE-MATCH-FN #'MATCH-PS-FILE) (INCLUDE-JS NIL) (WATCH-CHANGES NIL) (CLS 'PSLIB)

    Define new parenscript library. The ps source files may come from few
    different places described below.
    
    :system-source-dir should always be set unless you use only :runtime key
    (and no files at all), it should be equal to project source directory, and
    is used to ...
    
    :files is used to directly specify files that you want to be included
    into library, all of the included files should be located inside
    of :system-source-dir
    
    If :paren-source-dir key is provided it walks recursively through
    directory looking for any files with *PS-EXT* extension.
    If :inlcude-js option is set then it will also grab *JS-EXT* files.
    If :watch-changes options is set then it will look for changes
    inside :paren-source-dir
    
    Use :package to set package when compiling parenscript library.
    
    With :runtime key you can specify parenscript forms that can be
    included in the lib directly (without use of files).
    :file-match-fn lets you control how files are matched when compiling
    the library, look at match-file.

NOTE: Personally i usually start with defpslib form, because it
allows faster development cycle, adding/removing files from library
definion is easy. But when project gets mature enough i move
definition to asdf system, which makes deploying and sharing easier.

## How to compile a library

- [function] COMPILE-PSLIB LIB &KEY OUTPUT MINIFY

    Compile the whole lib returning javascript as a simple string.

- [function] COMPILE-PSFILE LIB FILENAME &KEY PACKAGE OUTPUT MINIFY

- [method] COMPILE-PSRUNTIME (LIB PSLIB)

    Compile lib runtime forms.

## How to serve it via web

- [function] ALL-FILES-URLS LIB &KEY HOST

    Build urls for all paren lib files, if host is provided it will
    output absolute urls if not then relative. Those relative urls should
    work with provided HUNCHENTOOT and RESTAS modules.

- [function] CONCRETE-FILES-URLS LIB FILES &KEY HOST

    Build urls for specific files, also look at ALL-FILES-URLS
