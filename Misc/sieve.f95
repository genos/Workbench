integer i,j,imax,prime_max
logical, allocatable :: primes(:)

write(*,*) 'Input largest number for search'
read(*,*) prime_max

allocate(primes(prime_max))
primes=.true. ! Sets all elements of array to 1
imax=sqrt(real(prime_max))

do i=2,imax 
    do j=2*i,prime_max,i
        primes(j)=.false.
    enddo
enddo

do i=2,prime_max 
    if (primes(i)) write(*,*)i
enddo
end
