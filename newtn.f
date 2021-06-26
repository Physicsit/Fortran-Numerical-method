        Implicit none
        real a(100),f,df,x
        integer n,z,k
        open(4,file='Newton')
  10    print*,'Enter the approximate value of function'
        write(4,*)'Enter the approximate value of fution'
        read*,a(1)
        write(3,*)a(1)
        write(*,*)'the function is'
        write(3,*)'the functionis'
        k=1
        do 1 n=1,100
        k=k+1
        a(n+1)=a(n)-f(a(n))(df(a(n)))
        if(abs(a(n+1)-a(n)).lt.0.0001)then
        write(*,*)'the correct value of the solution is',a(n+1)
        write(3,*)'the correct value of the solution is',a(n+1)
        goto 6
        end if
  1     continue
  6     write(*,*)'the iteration are'
        write(3,*)'the iteration are'
        do 5 n=1,k
        write(*,*)a(n)
        write(3,*)a(n)
  5     continue
        write(*,*)'if you want to continue'
        write(3,*)'if you want to continue'
        read*,z
        write(3,*)z
        IF(Z.EQ.1)GOTO 10
        Close(3)
        stop
        end
  !     Subprogram
        function f(x)
        f=x**2+3*x-6
        Return
        end
        function g(x)
        G=2*x+3
        return
        End
