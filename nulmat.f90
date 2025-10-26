subroutine nulmat(a, n)
    integer, intent (in)  :: n
    real, intent (inout) :: a(n)
    integer :: i

!$OMP PARALLEL DO
    do i= 1, n 
        a(i) = 0.
    enddo

end subroutine nulmat