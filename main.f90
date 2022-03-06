module config
    implicit none

    !Number of random points (improves precision, slows down the code)
    integer(8), parameter :: slw = 1d9 ! 1d9 takes around 8 seconds
    !Threads count
    integer, parameter :: thr = 4

contains

    pure real(8) function func(arg)
        real(8), intent(in) :: arg
        func = dcos(arg)
    end function

end module

program main
    use config
    implicit none
    abstract interface
        real(8) function fct(x)
        real(8), intent(in) :: x
        end function
    end interface

    procedure(fct), pointer :: ptr
    ptr => func
    call random_seed()

    print *, integ(ptr, 0d0, 1d0, slw), dsin(1d0)-dsin(0d0)


contains

    real(8) function integ(fptr, bt, tp, dots)

        integer(8), intent(in) :: dots
        real(8), intent(in) :: bt, tp
        procedure(fct), pointer, intent(in) :: fptr

        real(8) :: rnd
        integer(8) :: j

        integ = 0
        !$omp parallel default(none) shared(dots, fptr, bt, tp) private(j, rnd) num_threads(thr)
            !$omp do schedule(static) reduction(+: integ)
            do j = 1, dots
                call random_number(rnd)
                rnd = bt + (tp-bt)*rnd
                integ = integ + fptr(rnd)
            end do
            !$omp end do
        !$omp end parallel

        integ = (tp-bt)*integ/dots

    end function integ

end program main

