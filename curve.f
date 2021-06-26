        implicit none
        real sumx,x,y,sumy,sumxx,sumyy,sumx3,sumx4,sumx2y,x(100),d(3,4),
             y(100)
        integer n,i,j,k,u,a,b,c,d
        open(12,file='curve')
   10   write(*,*)'number of datas'
        write(12,*)'number of datas'
        read*,n
        write(12,*)n
        write(*,*)'x values'
        write(12,*)'x values'
        read*,(x(i),i=1,n)
        print*,'y values'
        write(12,*)'y values'
        read*,(y(i),i=1,n)
        write(12,*)(y(i),i=1,n)
        sumx=0.00
        sumy=0.00
        sumxx=0.00
        sumxy=0.00
        sumx3=0.00
        sumx4=0.00
        sumx2y=0.00
        DO 1 i=1,n,1
        sumx=sumx+x(i)
        sumy=sumy+y(i)
        sumxx=sumxx+x(i)**2
        sumx3=sumx3+x(i)**3
        sumx4=sumx4+x(i)**4
        sumx2y=sumx2y+x(i)**2*y(i)
   1    continue
        d(1,1)=sumx4
        d(1,2)=sumx3
        d(1,3)=sumxx
        d(1,4)=sumx2y
        d(2,1)=sumx3
        d(2,2)=sumxx
        d(2,3)=sumx
        d(3,1)=sumxx
        d(3,2)=sumxy
        d(3,3)=n
        d(3,4)=sumy
        a=0
        b=0
        c=0
        k=1,100,1
        a=(d(1,4)-d(1,3)*c-d(1,2)*b)/d(1,1)
        b=(d(2,4)-d(2,3)*c-d(2,1)*a)/d(2,2)
        c=(d(3,4)-d(3,1)*a-d(3,2)*b)/d(3,3)
        continue
        print*,'the value of a is',a,b,c
        write(*,*)'do you want to continue press 1'
        write(16,*)'do you want to continue press 1'
        read(*,*)u
        write(16,*)u
        if(u.eq.1)goto 10
        close(16)
        stop
        end
 
