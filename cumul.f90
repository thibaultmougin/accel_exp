subroutine cumul(a, k, b, c, n)
    integer, intent (in)  :: n
    real, intent (in)  :: b(n), c(n)
    real, intent (in)  :: k
    real, intent (inout) :: a(n)
    integer :: i

!$OMP PARALLEL DO
    do i= 1, n 
        a(i) = k*b(i) + c(i)
    enddo

end subroutine cumul