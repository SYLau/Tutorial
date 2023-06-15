!======================================================================
!Using subroutines and functions to separate the code into individual parts
!======================================================================
!subroutines and functions can be defined (1) locally or (2) in separate files in different ways
!(1) local subroutines/ functions let us better organize the main program structure and reuse certain lines
!(2) subroutines/ functions can be linked to the main program in different methods.
!The recommended method is to use modules (see module_example.f90).
!>Define subroutine/ functions in module_example.f90
!>Include "module_example.f90" in your project through: Project > Add files > module_example.f90
!>Before "implicit none", type "use module_example_mod" to link the module to the main program
!>(optional but recommended:) use "only:" to specify which subroutines/ functions you want to link

program using_subprograms
    use module_example_mod,only:subr_2,func_2
    implicit none
    real(8)::numb

    call subr_1('subr 1') ! calling local subroutine "subr_1"

    numb = func_1(4.d0) + func_1(2.5d0) ! using local function "func_1"
    print*, numb

    call subr_2(5.d0) ! calling subroutine "subr_2" from module "module_example_mod"

    numb = func_2(3.d2) ! using function "func_2" from module "module_example_mod"
    print*, numb

    !============================================================================
    ! We can define local subroutines and functions under "contains"
    !============================================================================
    contains

    subroutine subr_1(text)
        character(len=*),intent(in)::text
        print*, text
    end subroutine subr_1

    function func_1(x)
        real(8), intent(in)::x
        real(8) :: func_1
        func_1 = x*2
    end function func_1

end program

