! e_taylor.f95
! Calculates e^x via the Taylor Series approximation.
! Contemplated not using a function for factorial, and just adding up pieces
! along the way, but it got less accurate results.
!
! GRE, 2/26/10

program e_taylor
    implicit none

    real :: x, true_soln, factorial
    ! Input x; true (according to fortran) solution; function factorial
    integer :: n, i
    ! Input n: number of terms to take; dummy counter i
    real :: approx = 1.0
    ! Output approx: our approximated solution

    write(*,*) "I compute e^x via the Taylor Series approximation."

    write(*,*) "What value for x?"
    read(*,*) x
    true_soln = exp(x)

    write(*,*) "How far out should we take the approximation?"
    read(*,*) n

    do i = 1,n
        approx = approx + (x**i)/factorial(i)
        ! Add up terms of Taylor Series
    end do

    write(*,*) "My approximation:", approx          ! Output approximate answer
    write(*,*) "Actual answer:", true_soln          ! Output Fortran's solution
    write(*,*) "Error:", abs(true_soln - approx)    ! Output |Error|

end program e_taylor

function factorial(k)
! Computes k!
    implicit none

    real :: factorial                               ! Real to get more precision
    integer, intent(in) :: k                        ! Input
    integer :: j                                    ! Counter
    factorial = 1.0
    do j = 2,k
        factorial = factorial * j
    end do
    return
end function factorial
