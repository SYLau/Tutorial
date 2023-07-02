https://iraspa.org/blog/visual-studio-code-c-cpp-fortran-with-multiple-source-files/

Configuring Code Runner:
(1) In the settings, search for “executor map”, the relevant item is the “Code-runner: Executor Map”. Click “Edit in settings.json”.

(2) Replace the corresponding lines with these
"c": "cd \"$workspaceRoot\" ; make -f Makefile && ./myapp;",
"cpp": "cd \"$workspaceRoot\"; make -f Makefile && ./myapp;",
"FortranFreeForm": "cd \"$workspaceRoot\"; make -f Makefile && ./myapp;",
"fortran-modern": "cd \"$workspaceRoot\"; make -f Makefile && ./myapp;",
"fortran_fixed-form": "cd \"$workspaceRoot\"; make -f Makefile && ./myapp;",
"fortran": "cd \"$workspaceRoot\"; make -f Makefile && ./myapp;",
"makefile": "cd \"$workspaceRoot\"; make -f $fileName && ./myapp;",

Or [recommended]
(2) Don't change anything else. Just add the line:
"makefile": "cd $dir & make -f $fileName",

For Windows:

(3)  replace all ":" with "&". Change ./myapp to myapp

(4) With mingw32-make.exe installed instead, change "make" to "mingw32-make.exe" Or change the name of mingw32-make.exe to make.exe