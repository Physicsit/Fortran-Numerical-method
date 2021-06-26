        IMPlicit none
        integer i,j,k,n,u
        real a(10,10),b(10,10),c,f,x(10)
        open(16,File='gauss')
  10    print*,'Enter the number of rows'
        write(16,*)'Enter the number of rows'
        read*,n
        print*,n
        print*,'Enter the elements of augumented matrix'
        write(16,*)'Enter the elements of augumented matrix'
        do 1 i=1,n
        read*,(a(i,j),j=1,n+1)
        write(16,*)(a(i,j),j=1,n+1)
  1     continue
        do 2 k=1,n-1
        do 3 i=k+1,n
        f=a(i,k)/a(k,k)
        do 4 j=1,n+1
        a(i,j)=a(i,j)-f*a(k,j)
  4     continue
  3     continue
  2     continue
        do 5 i=1,n
        do 6 j=1,n+1
        b(i,j)=a(i,j)
  6     continue
  5     continue
        print*,'the forward elimination is'
        write(16,*)'the forward elimination is'
        do 15 i=1,n
        print*,(b(i,j),j=1,n+1)
        write(16,*)(b(i,j),j=1,n+1)
   15   continue
        x(n)=a(n,n+1)/a(n,n)
        print*,'the solution is'
        write(16,*)'the solution is'
        Print*,'z=',x(n)
        write(16,*)'z=',x(n)
        do 7 i=n-1,1,-1
        c=0.0
        do 8 j=i+1,n
        c=c+a(i,j)*x(j)
   8    continue
        x(i)=(a(i,n+1)-c)/a(i,i)
   7    continue
        If (n.gt.3) then
        do i=1,n
        print*,'x','(',i,')','=',x(i)
        write(16,*)'x','(',i,')','=',x(i)  
        enddo
        else        
        print*,'y=',x(n-1)
        write(16,*)'y=',x(n-1)
        print*,'x=',x(n-2)
        write(16,*),'x=',x(n-2)
        Endif
        write(*,*)'do you want to continue press 1'
        write(16,*)'do you want to continue press 1'
        read(*,*)u
        write(16,*)u
        if(u.eq.1)goto 10
        close(16)
        stop
        end

