      SUBROUTINE f_otfi(nr, nc, nl, i, dV, dt, Qd, a, p, V, zero)
            
      INTEGER nr, nc, nl, i, j
      DOUBLE PRECISION dV, dt, Qd(nl), a(nr), p(nr, nc), V(nl)
      DOUBLE PRECISION zero            
      DO 100 j = 2, nr
        dV = p(j,1)+p(j,2)-0.06*dt*Qd(i+1)
          
        IF ((p(j,4) .EQ. 1) .AND. (a(j-1) .LT. V(i+1))) THEN
           a(j) = (a(j-1)+dV)
        END IF   
        
        IF ((p(j,4) .EQ. 1) .AND. (a(j-1) .GE. V(i+1))) THEN
              a(j) = V(i+1)
        END IF

        IF ((p(j,4) .NE. 1) .AND. ((a(j-1)+dV) .LE. zero)) THEN
                   a(j) = 0
        END IF

        IF ((p(j,4) .NE. 1) .AND. ((a(j-1)+dV) .GT. zero)) THEN
                   a(j) = (a(j-1)+dV)
        END IF  
       
 100  CONTINUE
      END 
