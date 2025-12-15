      subroutine jpca15(ser,e)
c srampino adapts to standard ABC
      implicit real * 8 (a-h,o-z)
      dimension ser(3), der(3)
      r12=ser(1)
      r13=ser(2)
      r23=ser(3)
      call diat12(r12,e12,d12)
      call diat12(r13,e13,d13)
      call diat12(r23,e23,d23)
      call triaaa(r12,r13,r23,e123,der)
      e=e12+e13+e23+e123
      e=(e+0.1728d0)*27.21138386d0
      der(1)=d12+der(1)
      der(2)=d13+der(2)
      der(3)=d23+der(3)
      return
      end
************************************************************************
      subroutine diat12(r,ener,der)
************************************************************************
*     This subroutine computes the energies of a diatomic potential 
*     fitted to    10 points
*     rms =      0.12856952 kcal/mol
*     emax =      0.32188730 kcal/mol
************************************************************************
      implicit real*8 (a-h,o-z)
      dimension cf(  6)
      data cf(  1)/0.877523796221D+00/
      data cf(  2)/-.357378041560D+01/
      data cf(  3)/0.283330806286D+02/
      data cf(  4)/-.202250414440D+03/
      data cf(  5)/0.766086523737D+03/
      data cf(  6)/-.119728938668D+04/
      e0= 0.0000000D+00
      der=0.d0
      vex1= 0.1683700D+01
      vex2= 0.1413606D+01
      aux = 1.d0/r
      bux = dexp(-vex2*r)*aux
      cux = dexp(-vex1*r)
      ener=e0+cf(1)*bux
      dux=1.d0
      eux=r*cux
      do 1 i=2,  6
         der=der+(i-1)*cf(i)*dux
         dux=dux*eux
         ener=ener+cf(i)*dux
    1 continue
      der=der*(1.d0-vex1*r)*cux
      der=der-cf(1)*(vex2+aux)*bux
      return
      end
*************************************************************
      subroutine triaaa(r12,r13,r23,ener,der)       
*************************************************************
*     This subroutine computes the energies of a 3D PES     
*     for the AAA system class fitted to  276 points      
*     rms =      1.75035364 kcal/mol                              
*     emax =     12.92836324 kcal/mol                             
*************************************************************
      implicit real*8(a-h,o-z)                               
      dimension i1(   43),i2(   43),i3(   43),i4(   43),cf(   43)  
      dimension f12(0: 8),f13(0: 8),f23(0: 8)    
      dimension der(3)
      data cf(  1)/0.1870875561429140D+02/
      data i1(  1)/ 0/,i2(  1)/ 1/,i3(  1)/ 1/,i4(  1)/ 3/
      data cf(  2)/-.5733180030623026D+02/
      data i1(  2)/ 1/,i2(  2)/ 1/,i3(  2)/ 1/,i4(  2)/ 1/
      data cf(  3)/-.1183438478925001D+03/
      data i1(  3)/ 2/,i2(  3)/ 1/,i3(  3)/ 0/,i4(  3)/ 6/
      data cf(  4)/0.1947162365831663D+04/
      data i1(  4)/ 2/,i2(  4)/ 1/,i3(  4)/ 1/,i4(  4)/ 3/
      data cf(  5)/0.5712348700198917D+04/
      data i1(  5)/ 0/,i2(  5)/ 2/,i3(  5)/ 2/,i4(  5)/ 3/
      data cf(  6)/-.2787741468802820D+04/
      data i1(  6)/ 3/,i2(  6)/ 1/,i3(  6)/ 0/,i4(  6)/ 6/
      data cf(  7)/0.9297163986383715D+04/
      data i1(  7)/ 1/,i2(  7)/ 2/,i3(  7)/ 2/,i4(  7)/ 3/
      data cf(  8)/-.4652407675653481D+05/
      data i1(  8)/ 3/,i2(  8)/ 1/,i3(  8)/ 1/,i4(  8)/ 3/
      data cf(  9)/-.2442500599953606D+05/
      data i1(  9)/ 3/,i2(  9)/ 2/,i3(  9)/ 0/,i4(  9)/ 6/
      data cf( 10)/0.3806285816231862D+05/
      data i1( 10)/ 4/,i2( 10)/ 1/,i3( 10)/ 0/,i4( 10)/ 6/
      data cf( 11)/0.7088029987394271D+05/
      data i1( 11)/ 2/,i2( 11)/ 2/,i3( 11)/ 2/,i4( 11)/ 1/
      data cf( 12)/0.8490252504039127D+04/
      data i1( 12)/ 3/,i2( 12)/ 2/,i3( 12)/ 1/,i4( 12)/ 6/
      data cf( 13)/0.3929048522716801D+06/
      data i1( 13)/ 0/,i2( 13)/ 3/,i3( 13)/ 3/,i4( 13)/ 3/
      data cf( 14)/0.2218481290229143D+06/
      data i1( 14)/ 4/,i2( 14)/ 1/,i3( 14)/ 1/,i4( 14)/ 3/
      data cf( 15)/-.1456361270922911D+06/
      data i1( 15)/ 4/,i2( 15)/ 2/,i3( 15)/ 0/,i4( 15)/ 6/
      data cf( 16)/-.1697591353345037D+06/
      data i1( 16)/ 5/,i2( 16)/ 1/,i3( 16)/ 0/,i4( 16)/ 6/
      data cf( 17)/-.5528526334768089D+06/
      data i1( 17)/ 3/,i2( 17)/ 2/,i3( 17)/ 2/,i4( 17)/ 3/
      data cf( 18)/0.2329935237856133D+07/
      data i1( 18)/ 1/,i2( 18)/ 3/,i3( 18)/ 3/,i4( 18)/ 3/
      data cf( 19)/-.1405702489461415D+07/
      data i1( 19)/ 4/,i2( 19)/ 2/,i3( 19)/ 1/,i4( 19)/ 6/
      data cf( 20)/-.1001255718185193D+07/
      data i1( 20)/ 4/,i2( 20)/ 3/,i3( 20)/ 0/,i4( 20)/ 6/
      data cf( 21)/0.2720972417730024D+06/
      data i1( 21)/ 5/,i2( 21)/ 1/,i3( 21)/ 1/,i4( 21)/ 3/
      data cf( 22)/0.1094867480801619D+07/
      data i1( 22)/ 5/,i2( 22)/ 2/,i3( 22)/ 0/,i4( 22)/ 6/
      data cf( 23)/0.2795221037377880D+06/
      data i1( 23)/ 6/,i2( 23)/ 1/,i3( 23)/ 0/,i4( 23)/ 6/
      data cf( 24)/0.2310305509342059D+07/
      data i1( 24)/ 2/,i2( 24)/ 3/,i3( 24)/ 3/,i4( 24)/ 3/
      data cf( 25)/-.5887435932212975D+06/
      data i1( 25)/ 4/,i2( 25)/ 2/,i3( 25)/ 2/,i4( 25)/ 3/
      data cf( 26)/-.3682519902863689D+07/
      data i1( 26)/ 4/,i2( 26)/ 3/,i3( 26)/ 1/,i4( 26)/ 6/
      data cf( 27)/0.4496107024930726D+07/
      data i1( 27)/ 0/,i2( 27)/ 4/,i3( 27)/ 4/,i4( 27)/ 3/
      data cf( 28)/0.4599987903826319D+07/
      data i1( 28)/ 5/,i2( 28)/ 2/,i3( 28)/ 1/,i4( 28)/ 6/
      data cf( 29)/-.6299681930875555D+06/
      data i1( 29)/ 5/,i2( 29)/ 3/,i3( 29)/ 0/,i4( 29)/ 6/
      data cf( 30)/-.2749763547181666D+07/
      data i1( 30)/ 6/,i2( 30)/ 1/,i3( 30)/ 1/,i4( 30)/ 3/
      data cf( 31)/-.2023578990197911D+07/
      data i1( 31)/ 6/,i2( 31)/ 2/,i3( 31)/ 0/,i4( 31)/ 6/
      data cf( 32)/-.2179986551152479D+05/
      data i1( 32)/ 7/,i2( 32)/ 1/,i3( 32)/ 0/,i4( 32)/ 6/
      data cf( 33)/-.2485875723842572D+07/
      data i1( 33)/ 3/,i2( 33)/ 3/,i3( 33)/ 3/,i4( 33)/ 1/
      data cf( 34)/-.1064671300413286D+07/
      data i1( 34)/ 4/,i2( 34)/ 3/,i3( 34)/ 2/,i4( 34)/ 6/
      data cf( 35)/0.1284975483477602D+08/
      data i1( 35)/ 1/,i2( 35)/ 4/,i3( 35)/ 4/,i4( 35)/ 3/
      data cf( 36)/0.8006366869235807D+06/
      data i1( 36)/ 5/,i2( 36)/ 2/,i3( 36)/ 2/,i4( 36)/ 3/
      data cf( 37)/-.4788309198319245D+07/
      data i1( 37)/ 5/,i2( 37)/ 3/,i3( 37)/ 1/,i4( 37)/ 6/
      data cf( 38)/-.3234187909240489D+07/
      data i1( 38)/ 5/,i2( 38)/ 4/,i3( 38)/ 0/,i4( 38)/ 6/
      data cf( 39)/-.1908859023165694D+07/
      data i1( 39)/ 6/,i2( 39)/ 2/,i3( 39)/ 1/,i4( 39)/ 6/
      data cf( 40)/0.2529637138405075D+07/
      data i1( 40)/ 6/,i2( 40)/ 3/,i3( 40)/ 0/,i4( 40)/ 6/
      data cf( 41)/0.2804213441330383D+07/
      data i1( 41)/ 7/,i2( 41)/ 1/,i3( 41)/ 1/,i4( 41)/ 3/
      data cf( 42)/0.9057989317599398D+06/
      data i1( 42)/ 7/,i2( 42)/ 2/,i3( 42)/ 0/,i4( 42)/ 6/
      data cf( 43)/-.2157032368638654D+06/
      data i1( 43)/ 8/,i2( 43)/ 1/,i3( 43)/ 0/,i4( 43)/ 6/
      vex1=0.1245592726969979D+01
      f12(0)=1.d0
      f13(0)=1.d0
      f23(0)=1.d0
      bux12=r12*dexp(-vex1*r12)
      bux13=r13*dexp(-vex1*r13)
      bux23=r23*dexp(-vex1*r23)
      do 1 i=1, 8
         f12(i)=f12(i-1)*bux12
         f13(i)=f13(i-1)*bux13
         f23(i)=f23(i-1)*bux23
1     continue
      ener = 0.d0
      der12 = 0.d0
      der13 = 0.d0
      der23 = 0.d0
      do 2 l=1, 43
         if (i4(l).eq.1) then                             
            aux=f12(i1(l))*f13(i2(l))*f23(i3(l))          
            dux12=i1(l)*f12(i1(l)-1)*f13(i2(l))*f23(i3(l))
            dux13=i2(l)*f12(i1(l))*f13(i2(l)-1)*f23(i3(l))
            dux23=i3(l)*f12(i1(l))*f13(i2(l))*f23(i3(l)-1)
         elseif (i4(l).eq.3) then                         
            aux1=f12(i1(l))*f13(i2(l))*f23(i3(l))         
            aux2=f12(i3(l))*f13(i1(l))*f23(i2(l))         
            aux3=f12(i2(l))*f13(i3(l))*f23(i1(l))         
            aux=aux1+aux2+aux3                            
            dux1=i1(l)*f12(i1(l)-1)*f13(i2(l))*f23(i3(l)) 
            dux2=i3(l)*f12(i3(l)-1)*f13(i1(l))*f23(i2(l)) 
            dux3=i2(l)*f12(i2(l)-1)*f13(i3(l))*f23(i1(l)) 
            dux12=dux1+dux2+dux3                          
            dux1=i2(l)*f12(i1(l))*f13(i2(l)-1)*f23(i3(l)) 
            dux2=i1(l)*f12(i3(l))*f13(i1(l)-1)*f23(i2(l)) 
            dux3=i3(l)*f12(i2(l))*f13(i3(l)-1)*f23(i1(l)) 
            dux13=dux1+dux2+dux3                          
            dux1=i3(l)*f12(i1(l))*f13(i2(l))*f23(i3(l)-1) 
            dux2=i2(l)*f12(i3(l))*f13(i1(l))*f23(i2(l)-1) 
            dux3=i1(l)*f12(i2(l))*f13(i3(l))*f23(i1(l)-1) 
            dux23=dux1+dux2+dux3                          
         elseif (i4(l).eq.6) then                         
            aux1=f12(i1(l))*f13(i2(l))*f23(i3(l))         
            aux2=f12(i1(l))*f13(i3(l))*f23(i2(l))         
            aux3=f12(i2(l))*f13(i1(l))*f23(i3(l))         
            aux4=f12(i2(l))*f13(i3(l))*f23(i1(l))         
            aux5=f12(i3(l))*f13(i1(l))*f23(i2(l))         
            aux6=f12(i3(l))*f13(i2(l))*f23(i1(l))         
            aux=aux1+aux2+aux3+aux4+aux5+aux6             
            dux1=i1(l)*f12(i1(l)-1)*f13(i2(l))*f23(i3(l)) 
            dux2=i1(l)*f12(i1(l)-1)*f13(i3(l))*f23(i2(l)) 
            dux3=i2(l)*f12(i2(l)-1)*f13(i1(l))*f23(i3(l)) 
            dux4=i2(l)*f12(i2(l)-1)*f13(i3(l))*f23(i1(l)) 
            dux5=i3(l)*f12(i3(l)-1)*f13(i1(l))*f23(i2(l)) 
            dux6=i3(l)*f12(i3(l)-1)*f13(i2(l))*f23(i1(l)) 
            dux12=dux1+dux2+dux3+dux4+dux5+dux6           
            dux1=i2(l)*f12(i1(l))*f13(i2(l)-1)*f23(i3(l)) 
            dux2=i3(l)*f12(i1(l))*f13(i3(l)-1)*f23(i2(l)) 
            dux3=i1(l)*f12(i2(l))*f13(i1(l)-1)*f23(i3(l)) 
            dux4=i3(l)*f12(i2(l))*f13(i3(l)-1)*f23(i1(l)) 
            dux5=i1(l)*f12(i3(l))*f13(i1(l)-1)*f23(i2(l)) 
            dux6=i2(l)*f12(i3(l))*f13(i2(l)-1)*f23(i1(l)) 
            dux13=dux1+dux2+dux3+dux4+dux5+dux6           
            dux1=i3(l)*f12(i1(l))*f13(i2(l))*f23(i3(l)-1) 
            dux2=i2(l)*f12(i1(l))*f13(i3(l))*f23(i2(l)-1) 
            dux3=i3(l)*f12(i2(l))*f13(i1(l))*f23(i3(l)-1) 
            dux4=i1(l)*f12(i2(l))*f13(i3(l))*f23(i1(l)-1) 
            dux5=i2(l)*f12(i3(l))*f13(i1(l))*f23(i2(l)-1) 
            dux6=i1(l)*f12(i3(l))*f13(i2(l))*f23(i1(l)-1) 
            dux23=dux1+dux2+dux3+dux4+dux5+dux6           
         endif                                            
         ener=ener+cf(l)*aux                              
         der12=der12+cf(l)*dux12                          
         der13=der13+cf(l)*dux13                          
         der23=der23+cf(l)*dux23                          
    2 continue                                          
       der(1)=der12*(1.d0-vex1*r12)*dexp(-vex1*r12)       
       der(2)=der13*(1.d0-vex1*r13)*dexp(-vex1*r13)       
       der(3)=der23*(1.d0-vex1*r23)*dexp(-vex1*r23)       
      return
      end
