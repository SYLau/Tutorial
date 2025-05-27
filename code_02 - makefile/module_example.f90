module module_example_mod
    !As a good practice, always start a module with these 3 lines: implicit none, private, public::
    implicit none               !Suppress a Fortran obsolete feature that reserves implicit variables.
                                !For example, without this statement, "i" will be implicitly assumed to be an integer variable without declaration

    private                     !Make the content private.
                                !In large programs, different subroutines from different modules might crash if they are all publically accessible without specifications

    public::subr_2,func_2       !Define the publically available contents


    contains        !Define subroutines and functions (that are public) under "contains" that are accessible by any program that uses the module "module_example_mod"

    subroutine subr_2(x)
       real(8),intent(in)::x
        print*, x**3
        call subr_private
    end subroutine subr_2

    function func_2(x)
        real(8), intent(in)::x
        real(8) :: func_2
        func_2 = x*3 + x
    end function func_2

    subroutine subr_private        !An example of a private subroutine that cannot be used outside this module
        print*,'calling subr_private from subr_2:'
        print*,'  This is private to this module, since it is not declared as "public".'
        print*,'  Can only be called by the subroutines/ functions within this module'
    end subroutine subr_private

end module module_example_mod
