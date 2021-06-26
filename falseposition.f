        implicit none
        real a,b,x,error,f
	integer Count
        error=0.001
        open(3,file='False')
        print*,'the equation is in the form,f(x)'
        write(3,*),'the equation is in the form,f(x)'
  20    print*,'the value of a is'
        read*,a
        write(3,*),'the value of a is',a
        print*,'the value of b is'
        read*,b
        write(3,*),'the value of b is',b
        write(3,*),'--------------------------------------------'
        write(3,*),'a   b    x     f(x)'
  !     false position
        count=0
  30    if(f(A)*f(b).lt.0)then
        x=(a*f(b)-b*f(a))/(f(b)-f(A))
        else
        print*,'try with another value of and b'
        write(3,*),'try with another value of a and b'
        goto 20
        end if
        count=count+1
        if(f(A)*f(x).lt.0)then
        b=x
        else
        a=x
        endif
        print*,'the equation is in the form',x
        write(3,40)a,b,x,f(x)
  40   format(f8.5,5x,f8.5,5x,f8.6,5x,f10.9)
        if(abs(b-a).gt.error)goto 30 
        write(*,*),'the number of iteration performed is',Count 
        write(3,*),'the number of iteration performed is',Count
        print*,'the root is',x
        write(3,*)'the root is',x
        close(3)
        stop
        end
   !	Subprogram for f(x)
	real function f(x)
	implicit none
	real x
        f=x**2-2*x-1
        return
	end function
