        Implicit none
        integer i,j,row,column,a,b
        dimension a(10,10),b(10,10)
        open(10,file=
        print*,'Enter the numbers of rows and columns'
        write(10,*)'Enter the numbers of rows and column'
        read(5,*) row,column
        write(10,*) row, column
        print*,'Enter the elements of matrix A='
        write(10,*)'Enter the elements of matrix A='
        Do 1 i=1 row
        read (5,*) (a(i,j); j=1,column)
        print*,'matrix a='(a(i,j),column)
        write(10.*)(a(i,j),j=1,column)
        continue
        do 2 i=1,column
        do 3 j=1,row
        b(i,j)=a(j,i)
        continue
        continue
        print*'the transpose of matrix A='
        write(10,*)'the transpose of matrix A='
        Do 4 i=1, column
        print*(b(i,j,j=1,row)
        write(10,*)(b(i,j),j=1,row
        continue
