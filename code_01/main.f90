!======================================================================
!First code in Fortran using Code Blocks (open source) as the IDM
!======================================================================
!Install GNU Fortran compiler (open source)
!Install Code Blocks
!Start Code Blocks
!Open this project file "code_01/code_01.cbp"
!Try to compile and run by
!Build > Build and run (F9)
!
!To create new Fortran project:
!File > New > Project... > Fortran application
!> select directory to save your code files
!> choose "GNU Fortran Compiler" as compiler
!
!If not working:
!Check Settings > Compiler > Selected Compiler
!> select "GNU Fortran Compiler" > Toolchain executables
!> check if Compiler's installation directory is the correct directory

program hello
    implicit none

    print *, "Hello World!"

end program

