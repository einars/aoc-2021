inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w      inp w
mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0    mul x 0
add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z    add x z
mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26   mod x 26
div z 1    div z 1    div z 1    div z 26   div z 1    div z 26   div z 26   div z 26   div z 1    div z 26   div z 1    div z 1    div z 26   div z 26
add x 10   add x 13   add x 12   add x -12  add x 11   add x -13  add x -9   add x -12  add x 14   add x -9   add x 15   add x 11   add x -16  add x -2
eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w    eql x w
eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0    eql x 0
mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0
add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25   add y 25
mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x
add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1    add y 1
mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y    mul z y
mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0    mul y 0
add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w    add y w
add y 5    add y 9    add y 4    add y 4    add y 10   add y 14   add y 14   add y 12   add y 14   add y 14   add y 5    add y 10   add y 8    add y 15
mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x    mul y x
add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y    add z y

push +5   d0
push +9   d1
push +4   d2
pop -12   * d2 + 4 - 12 = d3
push +10  d4
pop -13   * d4 + 10 - 13 = d5
pop -9    * d1 + 9 - 9 = d6
pop -12   * d0 + 5 - 12 = d7
push +14  d8
pop -9    * d8 + 14 - 9 = d9
push +5   d10
push +10  d11
pop -16   * d11 + 10 - 16 = d12
pop -2    * d10 + 5 - 2 = d13

d0 - 7 = d7
d1 = d6
d2 - 8 = d3
d4 - 3 = d5
d8 + 5 = d9
d11 - 6 = d12
d10 + 3 = d13

01234567890123
99919692496939

01234567890123
81914111161714


