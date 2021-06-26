       implicit none
       real x,y,a,h,b,f,e,g
       integer i,j,n
       open(2,file='Euler')
       print*,'Enter the initial value of x and y'
       write(2,*)'Enter the initial value of x and y'
       read*,a,b
       write(2,*)a,b
       Print*,'Enter the value of interval'
       write(2,*)'Enter the value of interval'
       read*,h
       write(2,*)h
        Print*,'Enter the value of number of calculation'
        write(2,*)'Enter the value of number of calculation'
        Read*,n
        write(2,*)n
        print*,'Solution by basic euler method'
        write(2,*)'Solution by basic euler method'
        Print*,'x                            y'
        write(2,*)'x                            y'
        e=a
        g=b
        do 1 i=1,n
        x=h+a
        y=b+h*f(a,b)
        b=y
        a=x
        print*,x           ,   y
   1    continue
   3    format(f8.5,5x,f8.5)
        Print*,'Solution by improved method'
        write(2,*)'solution by improved method'
        print*,'x                y'
        write(2,*)'x                y'
        a=e
        b=g
        do 2 i=1,n
        x=h+a
        y=b+(h/2)*(f(a,b)+f(a+h,b+h*f(a,b)))
        b=y
        a=x
        print*,x           ,   y
        write(2,*)x             ,  y
   2    continue
        Print*,'Solution by modified method'
        write(2,*)'solution by modified method'
        print*,'x                y'
        write(2,*)'x                y'
        a=e
        b=g
        do 4 i=1,n
        x=h+a
        y=b+h*(f(a+(1/2)*h,b+(h/2)*f(a,b)))
        b=y
        a=x
        print*,x           ,   y
        write(2,*)x             ,  y
   4    continue
        close(2)
        stop
        end
  !     subprogram for F(a,b)
        real function f(a,b)
        implicit none
        real a,b
        f=(a+b)**(0.5)
        return
        end

        
