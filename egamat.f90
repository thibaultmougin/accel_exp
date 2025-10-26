subroutine egamat(a, b, n)
    integer, intent (in)  :: n
    real, intent (in)  :: b(n)
    real, intent (inout) :: a(n)
    integer :: i

!$OMP PARALLEL DO
    do i= 1, n 
        a(i) = b(i)
    enddo

end subroutine egamat