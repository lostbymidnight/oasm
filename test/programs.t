  $ oasm programs/basic.oasm | sed -E 's/\([0-9.]+ms\)/(TIME)/'
  
  SET (1)
   -> R0 (0)
   -> 42
  
  SET (2)
   -> R1 (0)
   -> 10
  
  PASS (3)
  
  ADD (4)
   -> R2 (42 + 10 = 52)
   -> R0 (42)
   -> R1 (10)
  
  HALT (5)
  
  R0 = 42
  R1 = 10
  R2 = 52
  R3 = 0
  R4 = 0
  R5 = 0
  R6 = 0
  R7 = 0
  
  Done! (TIME)

  $ oasm programs/final.oasm | sed -E 's/\([0-9.]+ms\)/(TIME)/'
  
  SET (1)
   -> R0 (0)
   -> 10
  
  SET (2)
   -> R1 (0)
   -> 20
  
  ADD (3)
   -> R2 (10 + 20 = 30)
   -> R0 (10)
   -> R1 (20)
  
  SET (4)
   -> R3 (0)
   -> 30
  
  ADD (5)
   -> R3 (30 + 30 = 60)
   -> R2 (30)
   -> R3 (30)
  
  SET (6)
   -> R4 (0)
   -> 255
  
  SET (7)
   -> R5 (0)
   -> 1
  
  ADD (8)
   -> R4 (255 + 1 = 0)
   -> R4 (255)
   -> R5 (1)
  
  SET (9)
   -> R6 (0)
   -> 128
  
  SET (10)
   -> R7 (0)
   -> 127
  
  ADD (11)
   -> R7 (128 + 127 = 255)
   -> R6 (128)
   -> R7 (127)
  
  JUMP (12)
   -> finish (14)
  
  PASS (15)
  
  HALT (16)
  
  R0 = 10
  R1 = 20
  R2 = 30
  R3 = 60
  R4 = 0
  R5 = 1
  R6 = 128
  R7 = 255
  
  Done! (TIME)

  $ oasm programs/extended.oasm | sed -E 's/\([0-9.]+ms\)/(TIME)/'
  
  SET (1)
   -> R0 (0)
   -> 3
  
  SET (2)
   -> R1 (0)
   -> 0
  
  ADD (3)
   -> R1 (0 + 3 = 3)
   -> R1 (0)
   -> R0 (3)
  
  DEC (4)
   -> R0 (3 - 1 = 2)
  
  COMPARE (5)
   -> R0 (2) vs 0
   -> flags (Z=0 C=0 N=0)
  
  IF_EQUAL (6)
   -> false (7)
  
  END (8)
   -> loop (2)
  
  ADD (3)
   -> R1 (3 + 2 = 5)
   -> R1 (3)
   -> R0 (2)
  
  DEC (4)
   -> R0 (2 - 1 = 1)
  
  COMPARE (5)
   -> R0 (1) vs 0
   -> flags (Z=0 C=0 N=0)
  
  IF_EQUAL (6)
   -> false (7)
  
  END (8)
   -> loop (2)
  
  ADD (3)
   -> R1 (5 + 1 = 6)
   -> R1 (5)
   -> R0 (1)
  
  DEC (4)
   -> R0 (1 - 1 = 0)
  
  COMPARE (5)
   -> R0 (0) vs 0
   -> flags (Z=1 C=0 N=0)
  
  IF_EQUAL (6)
   -> true
  
  BREAK (7)
   -> 8
  
  SET (9)
   -> R2 (0)
   -> 200
  
  WRITE (10)
   -> [200] (0 -> 6)
   -> R1 (6)
  
  READ (11)
   -> R3 (0)
   -> [200] = 6
  
  CALL (12)
   -> double (13)
   -> SP (254)
  
  ADD (14)
   -> R3 (6 + 6 = 12)
   -> R3 (6)
   -> R3 (6)
  
  RETURN (15)
   -> PC (12)
   -> SP (255)
  
  HALT (13)
  
  R0 = 0
  R1 = 6
  R2 = 200
  R3 = 12
  R4 = 0
  R5 = 0
  R6 = 0
  R7 = 0
  
  Done! (TIME)
