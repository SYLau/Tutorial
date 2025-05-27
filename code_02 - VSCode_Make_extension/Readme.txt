https://iraspa.org/blog/visual-studio-code-c-cpp-fortran-with-multiple-source-files/

Configuring Code Runner:
(1) In the settings, search for “executor map”, the relevant item is the “Code-runner: Executor Map”. Click “Edit in settings.json”.

(2) Replace the corresponding lines with these [Actually don't do this. Use the recommended one below.]
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

For windows, we can use bash if directory containing sh.exe or bash.exe is included in PATH: [recommended]
"makefile": "cd $dir & sh -c make -f $fileName",
Or if there are other sh.exe or bash.exe in other PATHs, or there is bash.exe existing under System32, then we should specify the path to sh or bash
"makefile": "cd $dir & C:\\msys64\\usr\\bin\\bash -c make -f $fileName",
This allows us to write the makefile in Linux command instead of windows.


For Windows:

(3)  replace all ":" with "&". Change ./myapp to myapp

(4) With mingw32-make.exe installed instead, change "make" to "mingw32-make.exe" Or change the name of mingw32-make.exe to make.exe


Note: Libraries
If a library is used, replace the line LIBS = xxxxx with the relative path of the library ".a" file