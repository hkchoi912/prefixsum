;buildInfoPackage: chisel3, version: 3.4.1, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit SparsePrefixSum : 
  module FA : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_1 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_2 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_3 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_4 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_6 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_7 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_8 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_9 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_10 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_5 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_6 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_7 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_8 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_9 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_10 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_1 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_11 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_12 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_13 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_14 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_15 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_11 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_12 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_13 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_14 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_15 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_16 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_17 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_18 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_19 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_20 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_21 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_16 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_17 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_18 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_19 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_20 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_21 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_2 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_3 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_22 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_23 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_24 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_25 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_26 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_22 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_23 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_24 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_25 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_26 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_27 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_28 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_29 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_30 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_31 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_32 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_27 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_28 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_29 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_30 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_31 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_32 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_2 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_4 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_5 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_33 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_34 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_35 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_36 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_37 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_6 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_33 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_34 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_35 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_36 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_37 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_38 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_39 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_40 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_41 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_42 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_43 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_7 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_38 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_39 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_40 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_41 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_42 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_43 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_3 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_6 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_7 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_44 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_45 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_46 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_47 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_48 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_8 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_44 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_45 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_46 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_47 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_48 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_49 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_50 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_51 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_52 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_53 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_54 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_9 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_49 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_50 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_51 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_52 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_53 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_54 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_4 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_8 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_9 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_55 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_56 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_57 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_58 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_59 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_10 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_55 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_56 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_57 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_58 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_59 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_60 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_61 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_62 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_63 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_64 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_65 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_11 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_60 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_61 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_62 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_63 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_64 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_65 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_5 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_10 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_11 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_66 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_67 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_68 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_69 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_70 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_12 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_66 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_67 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_68 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_69 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_70 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_71 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_72 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_73 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_74 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_75 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_76 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_13 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_71 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_72 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_73 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_74 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_75 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_76 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_6 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_12 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_13 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_77 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_78 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_79 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_80 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_81 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_14 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_77 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_78 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_79 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_80 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_81 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_82 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_83 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_84 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_85 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_86 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_87 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_15 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_82 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_83 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_84 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_85 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_86 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_87 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_7 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_14 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_15 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_88 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_89 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_90 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_91 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_92 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_16 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_88 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_89 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_90 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_91 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_92 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_93 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_94 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_95 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_96 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_97 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_98 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_17 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_93 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_94 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_95 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_96 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_97 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_98 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_8 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_16 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_17 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_99 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_100 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_101 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_102 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_103 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_18 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_99 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_100 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_101 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_102 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_103 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_104 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_105 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_106 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_107 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_108 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_109 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_19 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_104 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_105 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_106 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_107 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_108 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_109 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_9 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_18 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_19 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_110 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_111 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_112 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_113 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_114 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_20 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_110 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_111 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_112 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_113 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_114 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_115 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_116 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_117 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_118 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_119 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_120 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_21 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_115 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_116 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_117 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_118 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_119 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_120 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_10 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_20 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_21 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_121 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_122 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_123 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_124 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_125 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_22 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_121 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_122 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_123 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_124 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_125 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_126 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_127 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_128 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_129 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_130 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_131 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_23 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_126 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_127 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_128 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_129 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_130 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_131 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_11 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_22 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_23 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_132 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_133 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_134 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_135 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_136 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_24 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_132 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_133 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_134 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_135 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_136 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_137 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_138 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_139 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_140 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_141 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_142 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_25 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_137 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_138 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_139 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_140 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_141 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_142 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_12 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_24 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_25 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_143 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_144 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_145 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_146 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_147 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_26 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_143 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_144 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_145 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_146 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_147 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_148 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_149 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_150 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_151 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_152 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_153 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_27 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_148 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_149 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_150 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_151 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_152 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_153 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_13 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_26 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_27 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_154 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_155 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_156 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_157 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_158 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_28 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, sum : UInt<5>, cout : UInt<6>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_154 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_155 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_156 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_157 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_158 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[5] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[5] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    wire _WIRE_5 : UInt<1> @[test.scala 34:37]
    _WIRE_5 is invalid @[test.scala 34:37]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[6] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_5 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_15 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 3, 0) @[Bitwise.scala 109:18]
    node _T_17 = bits(_T_16, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_17, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_17, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_18 = bits(_T_16, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_18, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_18, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node lo_4 = bits(_T_15, 4, 4) @[Bitwise.scala 109:44]
    node _T_19 = cat(hi_4, lo_4) @[Cat.scala 30:58]
    io.sum <= _T_19 @[test.scala 46:10]
    node lo_hi = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node lo_5 = cat(lo_hi, Carry[5]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_5 = cat(hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node _T_20 = cat(hi_5, lo_5) @[Cat.scala 30:58]
    node _T_21 = bits(_T_20, 3, 0) @[Bitwise.scala 109:18]
    node _T_22 = bits(_T_21, 1, 0) @[Bitwise.scala 109:18]
    node hi_6 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_6 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node hi_7 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_23 = bits(_T_21, 3, 2) @[Bitwise.scala 109:44]
    node hi_8 = bits(_T_23, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_23, 1, 1) @[Bitwise.scala 109:44]
    node lo_8 = cat(hi_8, lo_7) @[Cat.scala 30:58]
    node hi_9 = cat(hi_7, lo_8) @[Cat.scala 30:58]
    node _T_24 = bits(_T_20, 5, 4) @[Bitwise.scala 109:44]
    node hi_10 = bits(_T_24, 0, 0) @[Bitwise.scala 109:18]
    node lo_9 = bits(_T_24, 1, 1) @[Bitwise.scala 109:44]
    node lo_10 = cat(hi_10, lo_9) @[Cat.scala 30:58]
    node _T_25 = cat(hi_9, lo_10) @[Cat.scala 30:58]
    io.cout <= _T_25 @[test.scala 47:11]
    
  module FA_159 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_160 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_161 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_162 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_163 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_164 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_29 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<6>, flip b : UInt<6>, flip c : UInt<6>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_159 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_160 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_161 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_162 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_163 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_164 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[6] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[6] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    wire _WIRE_6 : UInt<1> @[test.scala 34:37]
    _WIRE_6 is invalid @[test.scala 34:37]
    wire _WIRE_7 : UInt<1> @[test.scala 34:37]
    _WIRE_7 is invalid @[test.scala 34:37]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[7] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_6 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_7 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node lo_hi = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, Sum[5]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, Sum[2]) @[Cat.scala 30:58]
    node _T_18 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_19 = bits(_T_18, 3, 0) @[Bitwise.scala 109:18]
    node _T_20 = bits(_T_19, 1, 0) @[Bitwise.scala 109:18]
    node hi_1 = bits(_T_20, 0, 0) @[Bitwise.scala 109:18]
    node lo_1 = bits(_T_20, 1, 1) @[Bitwise.scala 109:44]
    node hi_2 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_21 = bits(_T_19, 3, 2) @[Bitwise.scala 109:44]
    node hi_3 = bits(_T_21, 0, 0) @[Bitwise.scala 109:18]
    node lo_2 = bits(_T_21, 1, 1) @[Bitwise.scala 109:44]
    node lo_3 = cat(hi_3, lo_2) @[Cat.scala 30:58]
    node hi_4 = cat(hi_2, lo_3) @[Cat.scala 30:58]
    node _T_22 = bits(_T_18, 5, 4) @[Bitwise.scala 109:44]
    node hi_5 = bits(_T_22, 0, 0) @[Bitwise.scala 109:18]
    node lo_4 = bits(_T_22, 1, 1) @[Bitwise.scala 109:44]
    node lo_5 = cat(hi_5, lo_4) @[Cat.scala 30:58]
    node _T_23 = cat(hi_4, lo_5) @[Cat.scala 30:58]
    io.sum <= _T_23 @[test.scala 46:10]
    node lo_hi_1 = cat(Carry[4], Carry[5]) @[Cat.scala 30:58]
    node lo_6 = cat(lo_hi_1, Carry[6]) @[Cat.scala 30:58]
    node hi_lo = cat(Carry[2], Carry[3]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_6 = cat(hi_hi_1, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi_6, lo_6) @[Cat.scala 30:58]
    node _T_25 = bits(_T_24, 3, 0) @[Bitwise.scala 109:18]
    node _T_26 = bits(_T_25, 1, 0) @[Bitwise.scala 109:18]
    node hi_7 = bits(_T_26, 0, 0) @[Bitwise.scala 109:18]
    node lo_7 = bits(_T_26, 1, 1) @[Bitwise.scala 109:44]
    node hi_8 = cat(hi_7, lo_7) @[Cat.scala 30:58]
    node _T_27 = bits(_T_25, 3, 2) @[Bitwise.scala 109:44]
    node hi_9 = bits(_T_27, 0, 0) @[Bitwise.scala 109:18]
    node lo_8 = bits(_T_27, 1, 1) @[Bitwise.scala 109:44]
    node lo_9 = cat(hi_9, lo_8) @[Cat.scala 30:58]
    node hi_10 = cat(hi_8, lo_9) @[Cat.scala 30:58]
    node _T_28 = bits(_T_24, 6, 4) @[Bitwise.scala 109:44]
    node _T_29 = bits(_T_28, 1, 0) @[Bitwise.scala 109:18]
    node hi_11 = bits(_T_29, 0, 0) @[Bitwise.scala 109:18]
    node lo_10 = bits(_T_29, 1, 1) @[Bitwise.scala 109:44]
    node hi_12 = cat(hi_11, lo_10) @[Cat.scala 30:58]
    node lo_11 = bits(_T_28, 2, 2) @[Bitwise.scala 109:44]
    node lo_12 = cat(hi_12, lo_11) @[Cat.scala 30:58]
    node _T_30 = cat(hi_10, lo_12) @[Cat.scala 30:58]
    io.cout <= _T_30 @[test.scala 47:11]
    
  module CSA4_14 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_28 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_29 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_165 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_166 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_167 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_168 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_169 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_170 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_171 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_172 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_30 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_165 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_166 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_167 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_168 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_169 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_170 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_171 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_172 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_173 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_174 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_175 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_176 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_177 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_178 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_179 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_180 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_181 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_31 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_173 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_174 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_175 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_176 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_177 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_178 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_179 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_180 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_181 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_15 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_30 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_31 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_182 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_183 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_184 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_185 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_186 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_187 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_188 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_189 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_32 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_182 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_183 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_184 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_185 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_186 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_187 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_188 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_189 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_190 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_191 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_192 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_193 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_194 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_195 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_196 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_197 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_198 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_33 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_190 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_191 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_192 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_193 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_194 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_195 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_196 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_197 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_198 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_16 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_32 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_33 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_199 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_200 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_201 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_202 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_203 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_204 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_205 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_206 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_34 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_199 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_200 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_201 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_202 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_203 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_204 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_205 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_206 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_207 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_208 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_209 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_210 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_211 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_212 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_213 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_214 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_215 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_35 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_207 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_208 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_209 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_210 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_211 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_212 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_213 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_214 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_215 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_17 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_34 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_35 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_216 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_217 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_218 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_219 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_220 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_221 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_222 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_223 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_36 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_216 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_217 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_218 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_219 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_220 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_221 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_222 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_223 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_224 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_225 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_226 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_227 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_228 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_229 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_230 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_231 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_232 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_37 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_224 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_225 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_226 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_227 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_228 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_229 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_230 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_231 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_232 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_18 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_36 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_37 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_233 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_234 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_235 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_236 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_237 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_238 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_239 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_240 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_38 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_233 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_234 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_235 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_236 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_237 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_238 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_239 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_240 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_241 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_242 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_243 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_244 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_245 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_246 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_247 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_248 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_249 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_39 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_241 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_242 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_243 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_244 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_245 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_246 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_247 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_248 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_249 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_19 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_38 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_39 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_250 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_251 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_252 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_253 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_254 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_255 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_256 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_257 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_40 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_250 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_251 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_252 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_253 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_254 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_255 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_256 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_257 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_258 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_259 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_260 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_261 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_262 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_263 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_264 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_265 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_266 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_41 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_258 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_259 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_260 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_261 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_262 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_263 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_264 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_265 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_266 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_20 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_40 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_41 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_267 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_268 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_269 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_270 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_271 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_272 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_273 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_274 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_42 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_267 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_268 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_269 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_270 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_271 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_272 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_273 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_274 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_275 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_276 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_277 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_278 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_279 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_280 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_281 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_282 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_283 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_43 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_275 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_276 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_277 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_278 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_279 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_280 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_281 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_282 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_283 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_21 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_42 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_43 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_284 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_285 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_286 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_287 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_288 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_289 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_290 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_291 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_44 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_284 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_285 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_286 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_287 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_288 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_289 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_290 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_291 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_292 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_293 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_294 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_295 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_296 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_297 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_298 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_299 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_300 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_45 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_292 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_293 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_294 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_295 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_296 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_297 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_298 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_299 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_300 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_22 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_44 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_45 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_301 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_302 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_303 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_304 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_305 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_306 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_307 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_308 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_46 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_301 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_302 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_303 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_304 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_305 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_306 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_307 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_308 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_309 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_310 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_311 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_312 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_313 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_314 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_315 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_316 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_317 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_47 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_309 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_310 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_311 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_312 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_313 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_314 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_315 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_316 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_317 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_23 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_46 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_47 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_318 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_319 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_320 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_321 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_322 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_323 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_324 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_325 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_48 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_318 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_319 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_320 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_321 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_322 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_323 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_324 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_325 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_326 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_327 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_328 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_329 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_330 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_331 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_332 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_333 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_334 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_49 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_326 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_327 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_328 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_329 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_330 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_331 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_332 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_333 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_334 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_24 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_48 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_49 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_335 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_336 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_337 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_338 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_339 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_340 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_341 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_342 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_50 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_335 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_336 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_337 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_338 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_339 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_340 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_341 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_342 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_343 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_344 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_345 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_346 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_347 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_348 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_349 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_350 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_351 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_51 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_343 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_344 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_345 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_346 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_347 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_348 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_349 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_350 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_351 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_25 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_50 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_51 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module FA_352 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_353 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_354 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_355 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_356 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_357 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_358 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_359 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_52 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, sum : UInt<8>, cout : UInt<9>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_352 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_353 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_354 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_355 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_356 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_357 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_358 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_359 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[8] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[8] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    wire _WIRE_8 : UInt<1> @[test.scala 34:37]
    _WIRE_8 is invalid @[test.scala 34:37]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[9] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_8 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[6], Sum[7]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[4], Sum[5]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[2], Sum[3]) @[Cat.scala 30:58]
    node hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_24 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_25 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_26 = xor(UInt<8>("h0ff"), _T_25) @[Bitwise.scala 102:21]
    node _T_27 = shr(_T_24, 4) @[Bitwise.scala 103:21]
    node _T_28 = and(_T_27, _T_26) @[Bitwise.scala 103:31]
    node _T_29 = bits(_T_24, 3, 0) @[Bitwise.scala 103:46]
    node _T_30 = shl(_T_29, 4) @[Bitwise.scala 103:65]
    node _T_31 = not(_T_26) @[Bitwise.scala 103:77]
    node _T_32 = and(_T_30, _T_31) @[Bitwise.scala 103:75]
    node _T_33 = or(_T_28, _T_32) @[Bitwise.scala 103:39]
    node _T_34 = bits(_T_26, 5, 0) @[Bitwise.scala 102:28]
    node _T_35 = shl(_T_34, 2) @[Bitwise.scala 102:47]
    node _T_36 = xor(_T_26, _T_35) @[Bitwise.scala 102:21]
    node _T_37 = shr(_T_33, 2) @[Bitwise.scala 103:21]
    node _T_38 = and(_T_37, _T_36) @[Bitwise.scala 103:31]
    node _T_39 = bits(_T_33, 5, 0) @[Bitwise.scala 103:46]
    node _T_40 = shl(_T_39, 2) @[Bitwise.scala 103:65]
    node _T_41 = not(_T_36) @[Bitwise.scala 103:77]
    node _T_42 = and(_T_40, _T_41) @[Bitwise.scala 103:75]
    node _T_43 = or(_T_38, _T_42) @[Bitwise.scala 103:39]
    node _T_44 = bits(_T_36, 6, 0) @[Bitwise.scala 102:28]
    node _T_45 = shl(_T_44, 1) @[Bitwise.scala 102:47]
    node _T_46 = xor(_T_36, _T_45) @[Bitwise.scala 102:21]
    node _T_47 = shr(_T_43, 1) @[Bitwise.scala 103:21]
    node _T_48 = and(_T_47, _T_46) @[Bitwise.scala 103:31]
    node _T_49 = bits(_T_43, 6, 0) @[Bitwise.scala 103:46]
    node _T_50 = shl(_T_49, 1) @[Bitwise.scala 103:65]
    node _T_51 = not(_T_46) @[Bitwise.scala 103:77]
    node _T_52 = and(_T_50, _T_51) @[Bitwise.scala 103:75]
    node _T_53 = or(_T_48, _T_52) @[Bitwise.scala 103:39]
    io.sum <= _T_53 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[7], Carry[8]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_1 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi, Carry[2]) @[Cat.scala 30:58]
    node hi_1 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_54 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _T_55 = bits(_T_54, 7, 0) @[Bitwise.scala 109:18]
    node _T_56 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_57 = xor(UInt<8>("h0ff"), _T_56) @[Bitwise.scala 102:21]
    node _T_58 = shr(_T_55, 4) @[Bitwise.scala 103:21]
    node _T_59 = and(_T_58, _T_57) @[Bitwise.scala 103:31]
    node _T_60 = bits(_T_55, 3, 0) @[Bitwise.scala 103:46]
    node _T_61 = shl(_T_60, 4) @[Bitwise.scala 103:65]
    node _T_62 = not(_T_57) @[Bitwise.scala 103:77]
    node _T_63 = and(_T_61, _T_62) @[Bitwise.scala 103:75]
    node _T_64 = or(_T_59, _T_63) @[Bitwise.scala 103:39]
    node _T_65 = bits(_T_57, 5, 0) @[Bitwise.scala 102:28]
    node _T_66 = shl(_T_65, 2) @[Bitwise.scala 102:47]
    node _T_67 = xor(_T_57, _T_66) @[Bitwise.scala 102:21]
    node _T_68 = shr(_T_64, 2) @[Bitwise.scala 103:21]
    node _T_69 = and(_T_68, _T_67) @[Bitwise.scala 103:31]
    node _T_70 = bits(_T_64, 5, 0) @[Bitwise.scala 103:46]
    node _T_71 = shl(_T_70, 2) @[Bitwise.scala 103:65]
    node _T_72 = not(_T_67) @[Bitwise.scala 103:77]
    node _T_73 = and(_T_71, _T_72) @[Bitwise.scala 103:75]
    node _T_74 = or(_T_69, _T_73) @[Bitwise.scala 103:39]
    node _T_75 = bits(_T_67, 6, 0) @[Bitwise.scala 102:28]
    node _T_76 = shl(_T_75, 1) @[Bitwise.scala 102:47]
    node _T_77 = xor(_T_67, _T_76) @[Bitwise.scala 102:21]
    node _T_78 = shr(_T_74, 1) @[Bitwise.scala 103:21]
    node _T_79 = and(_T_78, _T_77) @[Bitwise.scala 103:31]
    node _T_80 = bits(_T_74, 6, 0) @[Bitwise.scala 103:46]
    node _T_81 = shl(_T_80, 1) @[Bitwise.scala 103:65]
    node _T_82 = not(_T_77) @[Bitwise.scala 103:77]
    node _T_83 = and(_T_81, _T_82) @[Bitwise.scala 103:75]
    node hi_2 = or(_T_79, _T_83) @[Bitwise.scala 103:39]
    node lo_2 = bits(_T_54, 8, 8) @[Bitwise.scala 109:44]
    node _T_84 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    io.cout <= _T_84 @[test.scala 47:11]
    
  module FA_360 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_361 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_362 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_363 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_364 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_365 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_366 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_367 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module FA_368 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node a_xor_b = xor(io.a, io.b) @[test.scala 14:22]
    node _T = xor(a_xor_b, io.cin) @[test.scala 15:21]
    io.sum <= _T @[test.scala 15:10]
    node a_and_b = and(io.a, io.b) @[test.scala 17:22]
    node b_and_cin = and(io.b, io.cin) @[test.scala 18:24]
    node a_and_cin = and(io.a, io.cin) @[test.scala 19:24]
    node _T_1 = or(a_and_b, b_and_cin) @[test.scala 20:22]
    node _T_2 = or(_T_1, a_and_cin) @[test.scala 20:34]
    io.cout <= _T_2 @[test.scala 20:11]
    
  module CSA3_53 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<9>, flip b : UInt<9>, flip c : UInt<9>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst FA of FA_360 @[test.scala 32:33]
    FA.io is invalid
    FA.clock <= clock
    FA.reset <= reset
    inst FA_1 of FA_361 @[test.scala 32:33]
    FA_1.io is invalid
    FA_1.clock <= clock
    FA_1.reset <= reset
    inst FA_2 of FA_362 @[test.scala 32:33]
    FA_2.io is invalid
    FA_2.clock <= clock
    FA_2.reset <= reset
    inst FA_3 of FA_363 @[test.scala 32:33]
    FA_3.io is invalid
    FA_3.clock <= clock
    FA_3.reset <= reset
    inst FA_4 of FA_364 @[test.scala 32:33]
    FA_4.io is invalid
    FA_4.clock <= clock
    FA_4.reset <= reset
    inst FA_5 of FA_365 @[test.scala 32:33]
    FA_5.io is invalid
    FA_5.clock <= clock
    FA_5.reset <= reset
    inst FA_6 of FA_366 @[test.scala 32:33]
    FA_6.io is invalid
    FA_6.clock <= clock
    FA_6.reset <= reset
    inst FA_7 of FA_367 @[test.scala 32:33]
    FA_7.io is invalid
    FA_7.clock <= clock
    FA_7.reset <= reset
    inst FA_8 of FA_368 @[test.scala 32:33]
    FA_8.io is invalid
    FA_8.clock <= clock
    FA_8.reset <= reset
    wire FAs : {flip a : UInt<1>, flip b : UInt<1>, flip cin : UInt<1>, sum : UInt<1>, cout : UInt<1>}[9] @[compatibility.scala 127:12]
    FAs is invalid @[compatibility.scala 127:12]
    FAs[0] <- FA.io @[compatibility.scala 127:12]
    FAs[1] <- FA_1.io @[compatibility.scala 127:12]
    FAs[2] <- FA_2.io @[compatibility.scala 127:12]
    FAs[3] <- FA_3.io @[compatibility.scala 127:12]
    FAs[4] <- FA_4.io @[compatibility.scala 127:12]
    FAs[5] <- FA_5.io @[compatibility.scala 127:12]
    FAs[6] <- FA_6.io @[compatibility.scala 127:12]
    FAs[7] <- FA_7.io @[compatibility.scala 127:12]
    FAs[8] <- FA_8.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<1> @[test.scala 33:31]
    _WIRE is invalid @[test.scala 33:31]
    wire _WIRE_1 : UInt<1> @[test.scala 33:31]
    _WIRE_1 is invalid @[test.scala 33:31]
    wire _WIRE_2 : UInt<1> @[test.scala 33:31]
    _WIRE_2 is invalid @[test.scala 33:31]
    wire _WIRE_3 : UInt<1> @[test.scala 33:31]
    _WIRE_3 is invalid @[test.scala 33:31]
    wire _WIRE_4 : UInt<1> @[test.scala 33:31]
    _WIRE_4 is invalid @[test.scala 33:31]
    wire _WIRE_5 : UInt<1> @[test.scala 33:31]
    _WIRE_5 is invalid @[test.scala 33:31]
    wire _WIRE_6 : UInt<1> @[test.scala 33:31]
    _WIRE_6 is invalid @[test.scala 33:31]
    wire _WIRE_7 : UInt<1> @[test.scala 33:31]
    _WIRE_7 is invalid @[test.scala 33:31]
    wire _WIRE_8 : UInt<1> @[test.scala 33:31]
    _WIRE_8 is invalid @[test.scala 33:31]
    wire Sum : UInt<1>[9] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    wire _WIRE_9 : UInt<1> @[test.scala 34:37]
    _WIRE_9 is invalid @[test.scala 34:37]
    wire _WIRE_10 : UInt<1> @[test.scala 34:37]
    _WIRE_10 is invalid @[test.scala 34:37]
    wire _WIRE_11 : UInt<1> @[test.scala 34:37]
    _WIRE_11 is invalid @[test.scala 34:37]
    wire _WIRE_12 : UInt<1> @[test.scala 34:37]
    _WIRE_12 is invalid @[test.scala 34:37]
    wire _WIRE_13 : UInt<1> @[test.scala 34:37]
    _WIRE_13 is invalid @[test.scala 34:37]
    wire _WIRE_14 : UInt<1> @[test.scala 34:37]
    _WIRE_14 is invalid @[test.scala 34:37]
    wire _WIRE_15 : UInt<1> @[test.scala 34:37]
    _WIRE_15 is invalid @[test.scala 34:37]
    wire _WIRE_16 : UInt<1> @[test.scala 34:37]
    _WIRE_16 is invalid @[test.scala 34:37]
    wire _WIRE_17 : UInt<1> @[test.scala 34:37]
    _WIRE_17 is invalid @[test.scala 34:37]
    wire _WIRE_18 : UInt<1> @[test.scala 34:37]
    _WIRE_18 is invalid @[test.scala 34:37]
    wire Carry : UInt<1>[10] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_9 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_10 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_11 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_12 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_13 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_14 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_15 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[0] <= UInt<1>("h00") @[test.scala 36:12]
    node _T = bits(io.a, 0, 0) @[test.scala 39:21]
    FAs[0].a <= _T @[test.scala 39:14]
    node _T_1 = bits(io.b, 0, 0) @[test.scala 40:21]
    FAs[0].b <= _T_1 @[test.scala 40:14]
    node _T_2 = bits(io.c, 0, 0) @[test.scala 41:23]
    FAs[0].cin <= _T_2 @[test.scala 41:16]
    Sum[0] <= FAs[0].sum @[test.scala 42:12]
    Carry[1] <= FAs[0].cout @[test.scala 43:18]
    node _T_3 = bits(io.a, 1, 1) @[test.scala 39:21]
    FAs[1].a <= _T_3 @[test.scala 39:14]
    node _T_4 = bits(io.b, 1, 1) @[test.scala 40:21]
    FAs[1].b <= _T_4 @[test.scala 40:14]
    node _T_5 = bits(io.c, 1, 1) @[test.scala 41:23]
    FAs[1].cin <= _T_5 @[test.scala 41:16]
    Sum[1] <= FAs[1].sum @[test.scala 42:12]
    Carry[2] <= FAs[1].cout @[test.scala 43:18]
    node _T_6 = bits(io.a, 2, 2) @[test.scala 39:21]
    FAs[2].a <= _T_6 @[test.scala 39:14]
    node _T_7 = bits(io.b, 2, 2) @[test.scala 40:21]
    FAs[2].b <= _T_7 @[test.scala 40:14]
    node _T_8 = bits(io.c, 2, 2) @[test.scala 41:23]
    FAs[2].cin <= _T_8 @[test.scala 41:16]
    Sum[2] <= FAs[2].sum @[test.scala 42:12]
    Carry[3] <= FAs[2].cout @[test.scala 43:18]
    node _T_9 = bits(io.a, 3, 3) @[test.scala 39:21]
    FAs[3].a <= _T_9 @[test.scala 39:14]
    node _T_10 = bits(io.b, 3, 3) @[test.scala 40:21]
    FAs[3].b <= _T_10 @[test.scala 40:14]
    node _T_11 = bits(io.c, 3, 3) @[test.scala 41:23]
    FAs[3].cin <= _T_11 @[test.scala 41:16]
    Sum[3] <= FAs[3].sum @[test.scala 42:12]
    Carry[4] <= FAs[3].cout @[test.scala 43:18]
    node _T_12 = bits(io.a, 4, 4) @[test.scala 39:21]
    FAs[4].a <= _T_12 @[test.scala 39:14]
    node _T_13 = bits(io.b, 4, 4) @[test.scala 40:21]
    FAs[4].b <= _T_13 @[test.scala 40:14]
    node _T_14 = bits(io.c, 4, 4) @[test.scala 41:23]
    FAs[4].cin <= _T_14 @[test.scala 41:16]
    Sum[4] <= FAs[4].sum @[test.scala 42:12]
    Carry[5] <= FAs[4].cout @[test.scala 43:18]
    node _T_15 = bits(io.a, 5, 5) @[test.scala 39:21]
    FAs[5].a <= _T_15 @[test.scala 39:14]
    node _T_16 = bits(io.b, 5, 5) @[test.scala 40:21]
    FAs[5].b <= _T_16 @[test.scala 40:14]
    node _T_17 = bits(io.c, 5, 5) @[test.scala 41:23]
    FAs[5].cin <= _T_17 @[test.scala 41:16]
    Sum[5] <= FAs[5].sum @[test.scala 42:12]
    Carry[6] <= FAs[5].cout @[test.scala 43:18]
    node _T_18 = bits(io.a, 6, 6) @[test.scala 39:21]
    FAs[6].a <= _T_18 @[test.scala 39:14]
    node _T_19 = bits(io.b, 6, 6) @[test.scala 40:21]
    FAs[6].b <= _T_19 @[test.scala 40:14]
    node _T_20 = bits(io.c, 6, 6) @[test.scala 41:23]
    FAs[6].cin <= _T_20 @[test.scala 41:16]
    Sum[6] <= FAs[6].sum @[test.scala 42:12]
    Carry[7] <= FAs[6].cout @[test.scala 43:18]
    node _T_21 = bits(io.a, 7, 7) @[test.scala 39:21]
    FAs[7].a <= _T_21 @[test.scala 39:14]
    node _T_22 = bits(io.b, 7, 7) @[test.scala 40:21]
    FAs[7].b <= _T_22 @[test.scala 40:14]
    node _T_23 = bits(io.c, 7, 7) @[test.scala 41:23]
    FAs[7].cin <= _T_23 @[test.scala 41:16]
    Sum[7] <= FAs[7].sum @[test.scala 42:12]
    Carry[8] <= FAs[7].cout @[test.scala 43:18]
    node _T_24 = bits(io.a, 8, 8) @[test.scala 39:21]
    FAs[8].a <= _T_24 @[test.scala 39:14]
    node _T_25 = bits(io.b, 8, 8) @[test.scala 40:21]
    FAs[8].b <= _T_25 @[test.scala 40:14]
    node _T_26 = bits(io.c, 8, 8) @[test.scala 41:23]
    FAs[8].cin <= _T_26 @[test.scala 41:16]
    Sum[8] <= FAs[8].sum @[test.scala 42:12]
    Carry[9] <= FAs[8].cout @[test.scala 43:18]
    node lo_lo = cat(Sum[7], Sum[8]) @[Cat.scala 30:58]
    node lo_hi = cat(Sum[5], Sum[6]) @[Cat.scala 30:58]
    node lo = cat(lo_hi, lo_lo) @[Cat.scala 30:58]
    node hi_lo = cat(Sum[3], Sum[4]) @[Cat.scala 30:58]
    node hi_hi_hi = cat(Sum[0], Sum[1]) @[Cat.scala 30:58]
    node hi_hi = cat(hi_hi_hi, Sum[2]) @[Cat.scala 30:58]
    node hi = cat(hi_hi, hi_lo) @[Cat.scala 30:58]
    node _T_27 = cat(hi, lo) @[Cat.scala 30:58]
    node _T_28 = bits(_T_27, 7, 0) @[Bitwise.scala 109:18]
    node _T_29 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_30 = xor(UInt<8>("h0ff"), _T_29) @[Bitwise.scala 102:21]
    node _T_31 = shr(_T_28, 4) @[Bitwise.scala 103:21]
    node _T_32 = and(_T_31, _T_30) @[Bitwise.scala 103:31]
    node _T_33 = bits(_T_28, 3, 0) @[Bitwise.scala 103:46]
    node _T_34 = shl(_T_33, 4) @[Bitwise.scala 103:65]
    node _T_35 = not(_T_30) @[Bitwise.scala 103:77]
    node _T_36 = and(_T_34, _T_35) @[Bitwise.scala 103:75]
    node _T_37 = or(_T_32, _T_36) @[Bitwise.scala 103:39]
    node _T_38 = bits(_T_30, 5, 0) @[Bitwise.scala 102:28]
    node _T_39 = shl(_T_38, 2) @[Bitwise.scala 102:47]
    node _T_40 = xor(_T_30, _T_39) @[Bitwise.scala 102:21]
    node _T_41 = shr(_T_37, 2) @[Bitwise.scala 103:21]
    node _T_42 = and(_T_41, _T_40) @[Bitwise.scala 103:31]
    node _T_43 = bits(_T_37, 5, 0) @[Bitwise.scala 103:46]
    node _T_44 = shl(_T_43, 2) @[Bitwise.scala 103:65]
    node _T_45 = not(_T_40) @[Bitwise.scala 103:77]
    node _T_46 = and(_T_44, _T_45) @[Bitwise.scala 103:75]
    node _T_47 = or(_T_42, _T_46) @[Bitwise.scala 103:39]
    node _T_48 = bits(_T_40, 6, 0) @[Bitwise.scala 102:28]
    node _T_49 = shl(_T_48, 1) @[Bitwise.scala 102:47]
    node _T_50 = xor(_T_40, _T_49) @[Bitwise.scala 102:21]
    node _T_51 = shr(_T_47, 1) @[Bitwise.scala 103:21]
    node _T_52 = and(_T_51, _T_50) @[Bitwise.scala 103:31]
    node _T_53 = bits(_T_47, 6, 0) @[Bitwise.scala 103:46]
    node _T_54 = shl(_T_53, 1) @[Bitwise.scala 103:65]
    node _T_55 = not(_T_50) @[Bitwise.scala 103:77]
    node _T_56 = and(_T_54, _T_55) @[Bitwise.scala 103:75]
    node hi_1 = or(_T_52, _T_56) @[Bitwise.scala 103:39]
    node lo_1 = bits(_T_27, 8, 8) @[Bitwise.scala 109:44]
    node _T_57 = cat(hi_1, lo_1) @[Cat.scala 30:58]
    io.sum <= _T_57 @[test.scala 46:10]
    node lo_lo_1 = cat(Carry[8], Carry[9]) @[Cat.scala 30:58]
    node lo_hi_hi = cat(Carry[5], Carry[6]) @[Cat.scala 30:58]
    node lo_hi_1 = cat(lo_hi_hi, Carry[7]) @[Cat.scala 30:58]
    node lo_2 = cat(lo_hi_1, lo_lo_1) @[Cat.scala 30:58]
    node hi_lo_1 = cat(Carry[3], Carry[4]) @[Cat.scala 30:58]
    node hi_hi_hi_1 = cat(Carry[0], Carry[1]) @[Cat.scala 30:58]
    node hi_hi_1 = cat(hi_hi_hi_1, Carry[2]) @[Cat.scala 30:58]
    node hi_2 = cat(hi_hi_1, hi_lo_1) @[Cat.scala 30:58]
    node _T_58 = cat(hi_2, lo_2) @[Cat.scala 30:58]
    node _T_59 = bits(_T_58, 7, 0) @[Bitwise.scala 109:18]
    node _T_60 = shl(UInt<4>("h0f"), 4) @[Bitwise.scala 102:47]
    node _T_61 = xor(UInt<8>("h0ff"), _T_60) @[Bitwise.scala 102:21]
    node _T_62 = shr(_T_59, 4) @[Bitwise.scala 103:21]
    node _T_63 = and(_T_62, _T_61) @[Bitwise.scala 103:31]
    node _T_64 = bits(_T_59, 3, 0) @[Bitwise.scala 103:46]
    node _T_65 = shl(_T_64, 4) @[Bitwise.scala 103:65]
    node _T_66 = not(_T_61) @[Bitwise.scala 103:77]
    node _T_67 = and(_T_65, _T_66) @[Bitwise.scala 103:75]
    node _T_68 = or(_T_63, _T_67) @[Bitwise.scala 103:39]
    node _T_69 = bits(_T_61, 5, 0) @[Bitwise.scala 102:28]
    node _T_70 = shl(_T_69, 2) @[Bitwise.scala 102:47]
    node _T_71 = xor(_T_61, _T_70) @[Bitwise.scala 102:21]
    node _T_72 = shr(_T_68, 2) @[Bitwise.scala 103:21]
    node _T_73 = and(_T_72, _T_71) @[Bitwise.scala 103:31]
    node _T_74 = bits(_T_68, 5, 0) @[Bitwise.scala 103:46]
    node _T_75 = shl(_T_74, 2) @[Bitwise.scala 103:65]
    node _T_76 = not(_T_71) @[Bitwise.scala 103:77]
    node _T_77 = and(_T_75, _T_76) @[Bitwise.scala 103:75]
    node _T_78 = or(_T_73, _T_77) @[Bitwise.scala 103:39]
    node _T_79 = bits(_T_71, 6, 0) @[Bitwise.scala 102:28]
    node _T_80 = shl(_T_79, 1) @[Bitwise.scala 102:47]
    node _T_81 = xor(_T_71, _T_80) @[Bitwise.scala 102:21]
    node _T_82 = shr(_T_78, 1) @[Bitwise.scala 103:21]
    node _T_83 = and(_T_82, _T_81) @[Bitwise.scala 103:31]
    node _T_84 = bits(_T_78, 6, 0) @[Bitwise.scala 103:46]
    node _T_85 = shl(_T_84, 1) @[Bitwise.scala 103:65]
    node _T_86 = not(_T_81) @[Bitwise.scala 103:77]
    node _T_87 = and(_T_85, _T_86) @[Bitwise.scala 103:75]
    node hi_3 = or(_T_83, _T_87) @[Bitwise.scala 103:39]
    node _T_88 = bits(_T_58, 9, 8) @[Bitwise.scala 109:44]
    node hi_4 = bits(_T_88, 0, 0) @[Bitwise.scala 109:18]
    node lo_3 = bits(_T_88, 1, 1) @[Bitwise.scala 109:44]
    node lo_4 = cat(hi_4, lo_3) @[Cat.scala 30:58]
    node _T_89 = cat(hi_3, lo_4) @[Cat.scala 30:58]
    io.cout <= _T_89 @[test.scala 47:11]
    
  module CSA4_26 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA3 of CSA3_52 @[test.scala 60:23]
    CSA3.io is invalid
    CSA3.clock <= clock
    CSA3.reset <= reset
    CSA3.io.a <= io.a @[test.scala 61:13]
    CSA3.io.b <= io.b @[test.scala 62:13]
    CSA3.io.c <= io.c @[test.scala 63:13]
    inst CSA3_1 of CSA3_53 @[test.scala 65:23]
    CSA3_1.io is invalid
    CSA3_1.clock <= clock
    CSA3_1.reset <= reset
    CSA3_1.io.a <= io.d @[test.scala 66:13]
    CSA3_1.io.b <= CSA3.io.sum @[test.scala 67:13]
    CSA3_1.io.c <= CSA3.io.cout @[test.scala 68:13]
    io.sum <= CSA3_1.io.sum @[test.scala 70:10]
    io.cout <= CSA3_1.io.cout @[test.scala 71:11]
    
  module SparsePrefixSum : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in : UInt<16>, out : UInt<112>}
    
    clock is invalid
    reset is invalid
    io is invalid
    inst CSA4 of CSA4 @[test.scala 81:40]
    CSA4.io is invalid
    CSA4.clock <= clock
    CSA4.reset <= reset
    inst CSA4_1 of CSA4_1 @[test.scala 81:40]
    CSA4_1.io is invalid
    CSA4_1.clock <= clock
    CSA4_1.reset <= reset
    inst CSA4_2 of CSA4_2 @[test.scala 81:40]
    CSA4_2.io is invalid
    CSA4_2.clock <= clock
    CSA4_2.reset <= reset
    inst CSA4_3 of CSA4_3 @[test.scala 81:40]
    CSA4_3.io is invalid
    CSA4_3.clock <= clock
    CSA4_3.reset <= reset
    inst CSA4_4 of CSA4_4 @[test.scala 81:40]
    CSA4_4.io is invalid
    CSA4_4.clock <= clock
    CSA4_4.reset <= reset
    inst CSA4_5 of CSA4_5 @[test.scala 81:40]
    CSA4_5.io is invalid
    CSA4_5.clock <= clock
    CSA4_5.reset <= reset
    inst CSA4_6 of CSA4_6 @[test.scala 81:40]
    CSA4_6.io is invalid
    CSA4_6.clock <= clock
    CSA4_6.reset <= reset
    inst CSA4_7 of CSA4_7 @[test.scala 81:40]
    CSA4_7.io is invalid
    CSA4_7.clock <= clock
    CSA4_7.reset <= reset
    inst CSA4_8 of CSA4_8 @[test.scala 81:40]
    CSA4_8.io is invalid
    CSA4_8.clock <= clock
    CSA4_8.reset <= reset
    inst CSA4_9 of CSA4_9 @[test.scala 81:40]
    CSA4_9.io is invalid
    CSA4_9.clock <= clock
    CSA4_9.reset <= reset
    inst CSA4_10 of CSA4_10 @[test.scala 81:40]
    CSA4_10.io is invalid
    CSA4_10.clock <= clock
    CSA4_10.reset <= reset
    inst CSA4_11 of CSA4_11 @[test.scala 81:40]
    CSA4_11.io is invalid
    CSA4_11.clock <= clock
    CSA4_11.reset <= reset
    inst CSA4_12 of CSA4_12 @[test.scala 81:40]
    CSA4_12.io is invalid
    CSA4_12.clock <= clock
    CSA4_12.reset <= reset
    inst CSA4_13 of CSA4_13 @[test.scala 81:40]
    CSA4_13.io is invalid
    CSA4_13.clock <= clock
    CSA4_13.reset <= reset
    inst CSA4_14 of CSA4_14 @[test.scala 81:40]
    CSA4_14.io is invalid
    CSA4_14.clock <= clock
    CSA4_14.reset <= reset
    wire CSA_UP : {flip a : UInt<5>, flip b : UInt<5>, flip c : UInt<5>, flip d : UInt<5>, sum : UInt<6>, cout : UInt<7>}[15] @[compatibility.scala 127:12]
    CSA_UP is invalid @[compatibility.scala 127:12]
    CSA_UP[0] <- CSA4.io @[compatibility.scala 127:12]
    CSA_UP[1] <- CSA4_1.io @[compatibility.scala 127:12]
    CSA_UP[2] <- CSA4_2.io @[compatibility.scala 127:12]
    CSA_UP[3] <- CSA4_3.io @[compatibility.scala 127:12]
    CSA_UP[4] <- CSA4_4.io @[compatibility.scala 127:12]
    CSA_UP[5] <- CSA4_5.io @[compatibility.scala 127:12]
    CSA_UP[6] <- CSA4_6.io @[compatibility.scala 127:12]
    CSA_UP[7] <- CSA4_7.io @[compatibility.scala 127:12]
    CSA_UP[8] <- CSA4_8.io @[compatibility.scala 127:12]
    CSA_UP[9] <- CSA4_9.io @[compatibility.scala 127:12]
    CSA_UP[10] <- CSA4_10.io @[compatibility.scala 127:12]
    CSA_UP[11] <- CSA4_11.io @[compatibility.scala 127:12]
    CSA_UP[12] <- CSA4_12.io @[compatibility.scala 127:12]
    CSA_UP[13] <- CSA4_13.io @[compatibility.scala 127:12]
    CSA_UP[14] <- CSA4_14.io @[compatibility.scala 127:12]
    inst CSA4_15 of CSA4_15 @[test.scala 83:11]
    CSA4_15.io is invalid
    CSA4_15.clock <= clock
    CSA4_15.reset <= reset
    inst CSA4_16 of CSA4_16 @[test.scala 83:11]
    CSA4_16.io is invalid
    CSA4_16.clock <= clock
    CSA4_16.reset <= reset
    inst CSA4_17 of CSA4_17 @[test.scala 83:11]
    CSA4_17.io is invalid
    CSA4_17.clock <= clock
    CSA4_17.reset <= reset
    inst CSA4_18 of CSA4_18 @[test.scala 83:11]
    CSA4_18.io is invalid
    CSA4_18.clock <= clock
    CSA4_18.reset <= reset
    inst CSA4_19 of CSA4_19 @[test.scala 83:11]
    CSA4_19.io is invalid
    CSA4_19.clock <= clock
    CSA4_19.reset <= reset
    inst CSA4_20 of CSA4_20 @[test.scala 83:11]
    CSA4_20.io is invalid
    CSA4_20.clock <= clock
    CSA4_20.reset <= reset
    inst CSA4_21 of CSA4_21 @[test.scala 83:11]
    CSA4_21.io is invalid
    CSA4_21.clock <= clock
    CSA4_21.reset <= reset
    inst CSA4_22 of CSA4_22 @[test.scala 83:11]
    CSA4_22.io is invalid
    CSA4_22.clock <= clock
    CSA4_22.reset <= reset
    inst CSA4_23 of CSA4_23 @[test.scala 83:11]
    CSA4_23.io is invalid
    CSA4_23.clock <= clock
    CSA4_23.reset <= reset
    inst CSA4_24 of CSA4_24 @[test.scala 83:11]
    CSA4_24.io is invalid
    CSA4_24.clock <= clock
    CSA4_24.reset <= reset
    inst CSA4_25 of CSA4_25 @[test.scala 83:11]
    CSA4_25.io is invalid
    CSA4_25.clock <= clock
    CSA4_25.reset <= reset
    inst CSA4_26 of CSA4_26 @[test.scala 83:11]
    CSA4_26.io is invalid
    CSA4_26.clock <= clock
    CSA4_26.reset <= reset
    wire CSA_DOWN : {flip a : UInt<8>, flip b : UInt<8>, flip c : UInt<8>, flip d : UInt<8>, sum : UInt<9>, cout : UInt<10>}[12] @[compatibility.scala 127:12]
    CSA_DOWN is invalid @[compatibility.scala 127:12]
    CSA_DOWN[0] <- CSA4_15.io @[compatibility.scala 127:12]
    CSA_DOWN[1] <- CSA4_16.io @[compatibility.scala 127:12]
    CSA_DOWN[2] <- CSA4_17.io @[compatibility.scala 127:12]
    CSA_DOWN[3] <- CSA4_18.io @[compatibility.scala 127:12]
    CSA_DOWN[4] <- CSA4_19.io @[compatibility.scala 127:12]
    CSA_DOWN[5] <- CSA4_20.io @[compatibility.scala 127:12]
    CSA_DOWN[6] <- CSA4_21.io @[compatibility.scala 127:12]
    CSA_DOWN[7] <- CSA4_22.io @[compatibility.scala 127:12]
    CSA_DOWN[8] <- CSA4_23.io @[compatibility.scala 127:12]
    CSA_DOWN[9] <- CSA4_24.io @[compatibility.scala 127:12]
    CSA_DOWN[10] <- CSA4_25.io @[compatibility.scala 127:12]
    CSA_DOWN[11] <- CSA4_26.io @[compatibility.scala 127:12]
    wire _WIRE : UInt<8> @[test.scala 85:31]
    _WIRE is invalid @[test.scala 85:31]
    wire _WIRE_1 : UInt<8> @[test.scala 85:31]
    _WIRE_1 is invalid @[test.scala 85:31]
    wire _WIRE_2 : UInt<8> @[test.scala 85:31]
    _WIRE_2 is invalid @[test.scala 85:31]
    wire _WIRE_3 : UInt<8> @[test.scala 85:31]
    _WIRE_3 is invalid @[test.scala 85:31]
    wire _WIRE_4 : UInt<8> @[test.scala 85:31]
    _WIRE_4 is invalid @[test.scala 85:31]
    wire _WIRE_5 : UInt<8> @[test.scala 85:31]
    _WIRE_5 is invalid @[test.scala 85:31]
    wire _WIRE_6 : UInt<8> @[test.scala 85:31]
    _WIRE_6 is invalid @[test.scala 85:31]
    wire _WIRE_7 : UInt<8> @[test.scala 85:31]
    _WIRE_7 is invalid @[test.scala 85:31]
    wire _WIRE_8 : UInt<8> @[test.scala 85:31]
    _WIRE_8 is invalid @[test.scala 85:31]
    wire _WIRE_9 : UInt<8> @[test.scala 85:31]
    _WIRE_9 is invalid @[test.scala 85:31]
    wire _WIRE_10 : UInt<8> @[test.scala 85:31]
    _WIRE_10 is invalid @[test.scala 85:31]
    wire _WIRE_11 : UInt<8> @[test.scala 85:31]
    _WIRE_11 is invalid @[test.scala 85:31]
    wire _WIRE_12 : UInt<8> @[test.scala 85:31]
    _WIRE_12 is invalid @[test.scala 85:31]
    wire _WIRE_13 : UInt<8> @[test.scala 85:31]
    _WIRE_13 is invalid @[test.scala 85:31]
    wire _WIRE_14 : UInt<8> @[test.scala 85:31]
    _WIRE_14 is invalid @[test.scala 85:31]
    wire _WIRE_15 : UInt<8> @[test.scala 85:31]
    _WIRE_15 is invalid @[test.scala 85:31]
    wire Sum : UInt<8>[16] @[compatibility.scala 127:12]
    Sum is invalid @[compatibility.scala 127:12]
    Sum[0] <= _WIRE @[compatibility.scala 127:12]
    Sum[1] <= _WIRE_1 @[compatibility.scala 127:12]
    Sum[2] <= _WIRE_2 @[compatibility.scala 127:12]
    Sum[3] <= _WIRE_3 @[compatibility.scala 127:12]
    Sum[4] <= _WIRE_4 @[compatibility.scala 127:12]
    Sum[5] <= _WIRE_5 @[compatibility.scala 127:12]
    Sum[6] <= _WIRE_6 @[compatibility.scala 127:12]
    Sum[7] <= _WIRE_7 @[compatibility.scala 127:12]
    Sum[8] <= _WIRE_8 @[compatibility.scala 127:12]
    Sum[9] <= _WIRE_9 @[compatibility.scala 127:12]
    Sum[10] <= _WIRE_10 @[compatibility.scala 127:12]
    Sum[11] <= _WIRE_11 @[compatibility.scala 127:12]
    Sum[12] <= _WIRE_12 @[compatibility.scala 127:12]
    Sum[13] <= _WIRE_13 @[compatibility.scala 127:12]
    Sum[14] <= _WIRE_14 @[compatibility.scala 127:12]
    Sum[15] <= _WIRE_15 @[compatibility.scala 127:12]
    wire _WIRE_16 : UInt<8> @[test.scala 86:37]
    _WIRE_16 is invalid @[test.scala 86:37]
    wire _WIRE_17 : UInt<8> @[test.scala 86:37]
    _WIRE_17 is invalid @[test.scala 86:37]
    wire _WIRE_18 : UInt<8> @[test.scala 86:37]
    _WIRE_18 is invalid @[test.scala 86:37]
    wire _WIRE_19 : UInt<8> @[test.scala 86:37]
    _WIRE_19 is invalid @[test.scala 86:37]
    wire _WIRE_20 : UInt<8> @[test.scala 86:37]
    _WIRE_20 is invalid @[test.scala 86:37]
    wire _WIRE_21 : UInt<8> @[test.scala 86:37]
    _WIRE_21 is invalid @[test.scala 86:37]
    wire _WIRE_22 : UInt<8> @[test.scala 86:37]
    _WIRE_22 is invalid @[test.scala 86:37]
    wire _WIRE_23 : UInt<8> @[test.scala 86:37]
    _WIRE_23 is invalid @[test.scala 86:37]
    wire _WIRE_24 : UInt<8> @[test.scala 86:37]
    _WIRE_24 is invalid @[test.scala 86:37]
    wire _WIRE_25 : UInt<8> @[test.scala 86:37]
    _WIRE_25 is invalid @[test.scala 86:37]
    wire _WIRE_26 : UInt<8> @[test.scala 86:37]
    _WIRE_26 is invalid @[test.scala 86:37]
    wire _WIRE_27 : UInt<8> @[test.scala 86:37]
    _WIRE_27 is invalid @[test.scala 86:37]
    wire _WIRE_28 : UInt<8> @[test.scala 86:37]
    _WIRE_28 is invalid @[test.scala 86:37]
    wire _WIRE_29 : UInt<8> @[test.scala 86:37]
    _WIRE_29 is invalid @[test.scala 86:37]
    wire _WIRE_30 : UInt<8> @[test.scala 86:37]
    _WIRE_30 is invalid @[test.scala 86:37]
    wire _WIRE_31 : UInt<8> @[test.scala 86:37]
    _WIRE_31 is invalid @[test.scala 86:37]
    wire _WIRE_32 : UInt<8> @[test.scala 86:37]
    _WIRE_32 is invalid @[test.scala 86:37]
    wire Carry : UInt<8>[17] @[compatibility.scala 127:12]
    Carry is invalid @[compatibility.scala 127:12]
    Carry[0] <= _WIRE_16 @[compatibility.scala 127:12]
    Carry[1] <= _WIRE_17 @[compatibility.scala 127:12]
    Carry[2] <= _WIRE_18 @[compatibility.scala 127:12]
    Carry[3] <= _WIRE_19 @[compatibility.scala 127:12]
    Carry[4] <= _WIRE_20 @[compatibility.scala 127:12]
    Carry[5] <= _WIRE_21 @[compatibility.scala 127:12]
    Carry[6] <= _WIRE_22 @[compatibility.scala 127:12]
    Carry[7] <= _WIRE_23 @[compatibility.scala 127:12]
    Carry[8] <= _WIRE_24 @[compatibility.scala 127:12]
    Carry[9] <= _WIRE_25 @[compatibility.scala 127:12]
    Carry[10] <= _WIRE_26 @[compatibility.scala 127:12]
    Carry[11] <= _WIRE_27 @[compatibility.scala 127:12]
    Carry[12] <= _WIRE_28 @[compatibility.scala 127:12]
    Carry[13] <= _WIRE_29 @[compatibility.scala 127:12]
    Carry[14] <= _WIRE_30 @[compatibility.scala 127:12]
    Carry[15] <= _WIRE_31 @[compatibility.scala 127:12]
    Carry[16] <= _WIRE_32 @[compatibility.scala 127:12]
    wire _WIRE_33 : UInt<8> @[test.scala 87:35]
    _WIRE_33 is invalid @[test.scala 87:35]
    wire _WIRE_34 : UInt<8> @[test.scala 87:35]
    _WIRE_34 is invalid @[test.scala 87:35]
    wire _WIRE_35 : UInt<8> @[test.scala 87:35]
    _WIRE_35 is invalid @[test.scala 87:35]
    wire _WIRE_36 : UInt<8> @[test.scala 87:35]
    _WIRE_36 is invalid @[test.scala 87:35]
    wire _WIRE_37 : UInt<8> @[test.scala 87:35]
    _WIRE_37 is invalid @[test.scala 87:35]
    wire _WIRE_38 : UInt<8> @[test.scala 87:35]
    _WIRE_38 is invalid @[test.scala 87:35]
    wire _WIRE_39 : UInt<8> @[test.scala 87:35]
    _WIRE_39 is invalid @[test.scala 87:35]
    wire _WIRE_40 : UInt<8> @[test.scala 87:35]
    _WIRE_40 is invalid @[test.scala 87:35]
    wire _WIRE_41 : UInt<8> @[test.scala 87:35]
    _WIRE_41 is invalid @[test.scala 87:35]
    wire _WIRE_42 : UInt<8> @[test.scala 87:35]
    _WIRE_42 is invalid @[test.scala 87:35]
    wire _WIRE_43 : UInt<8> @[test.scala 87:35]
    _WIRE_43 is invalid @[test.scala 87:35]
    wire _WIRE_44 : UInt<8> @[test.scala 87:35]
    _WIRE_44 is invalid @[test.scala 87:35]
    wire _WIRE_45 : UInt<8> @[test.scala 87:35]
    _WIRE_45 is invalid @[test.scala 87:35]
    wire _WIRE_46 : UInt<8> @[test.scala 87:35]
    _WIRE_46 is invalid @[test.scala 87:35]
    wire _WIRE_47 : UInt<8> @[test.scala 87:35]
    _WIRE_47 is invalid @[test.scala 87:35]
    wire _WIRE_48 : UInt<8> @[test.scala 87:35]
    _WIRE_48 is invalid @[test.scala 87:35]
    wire Sum_lv2 : UInt<8>[16] @[compatibility.scala 127:12]
    Sum_lv2 is invalid @[compatibility.scala 127:12]
    Sum_lv2[0] <= _WIRE_33 @[compatibility.scala 127:12]
    Sum_lv2[1] <= _WIRE_34 @[compatibility.scala 127:12]
    Sum_lv2[2] <= _WIRE_35 @[compatibility.scala 127:12]
    Sum_lv2[3] <= _WIRE_36 @[compatibility.scala 127:12]
    Sum_lv2[4] <= _WIRE_37 @[compatibility.scala 127:12]
    Sum_lv2[5] <= _WIRE_38 @[compatibility.scala 127:12]
    Sum_lv2[6] <= _WIRE_39 @[compatibility.scala 127:12]
    Sum_lv2[7] <= _WIRE_40 @[compatibility.scala 127:12]
    Sum_lv2[8] <= _WIRE_41 @[compatibility.scala 127:12]
    Sum_lv2[9] <= _WIRE_42 @[compatibility.scala 127:12]
    Sum_lv2[10] <= _WIRE_43 @[compatibility.scala 127:12]
    Sum_lv2[11] <= _WIRE_44 @[compatibility.scala 127:12]
    Sum_lv2[12] <= _WIRE_45 @[compatibility.scala 127:12]
    Sum_lv2[13] <= _WIRE_46 @[compatibility.scala 127:12]
    Sum_lv2[14] <= _WIRE_47 @[compatibility.scala 127:12]
    Sum_lv2[15] <= _WIRE_48 @[compatibility.scala 127:12]
    wire _WIRE_49 : UInt<8> @[test.scala 88:41]
    _WIRE_49 is invalid @[test.scala 88:41]
    wire _WIRE_50 : UInt<8> @[test.scala 88:41]
    _WIRE_50 is invalid @[test.scala 88:41]
    wire _WIRE_51 : UInt<8> @[test.scala 88:41]
    _WIRE_51 is invalid @[test.scala 88:41]
    wire _WIRE_52 : UInt<8> @[test.scala 88:41]
    _WIRE_52 is invalid @[test.scala 88:41]
    wire _WIRE_53 : UInt<8> @[test.scala 88:41]
    _WIRE_53 is invalid @[test.scala 88:41]
    wire _WIRE_54 : UInt<8> @[test.scala 88:41]
    _WIRE_54 is invalid @[test.scala 88:41]
    wire _WIRE_55 : UInt<8> @[test.scala 88:41]
    _WIRE_55 is invalid @[test.scala 88:41]
    wire _WIRE_56 : UInt<8> @[test.scala 88:41]
    _WIRE_56 is invalid @[test.scala 88:41]
    wire _WIRE_57 : UInt<8> @[test.scala 88:41]
    _WIRE_57 is invalid @[test.scala 88:41]
    wire _WIRE_58 : UInt<8> @[test.scala 88:41]
    _WIRE_58 is invalid @[test.scala 88:41]
    wire _WIRE_59 : UInt<8> @[test.scala 88:41]
    _WIRE_59 is invalid @[test.scala 88:41]
    wire _WIRE_60 : UInt<8> @[test.scala 88:41]
    _WIRE_60 is invalid @[test.scala 88:41]
    wire _WIRE_61 : UInt<8> @[test.scala 88:41]
    _WIRE_61 is invalid @[test.scala 88:41]
    wire _WIRE_62 : UInt<8> @[test.scala 88:41]
    _WIRE_62 is invalid @[test.scala 88:41]
    wire _WIRE_63 : UInt<8> @[test.scala 88:41]
    _WIRE_63 is invalid @[test.scala 88:41]
    wire _WIRE_64 : UInt<8> @[test.scala 88:41]
    _WIRE_64 is invalid @[test.scala 88:41]
    wire _WIRE_65 : UInt<8> @[test.scala 88:41]
    _WIRE_65 is invalid @[test.scala 88:41]
    wire Carry_lv2 : UInt<8>[17] @[compatibility.scala 127:12]
    Carry_lv2 is invalid @[compatibility.scala 127:12]
    Carry_lv2[0] <= _WIRE_49 @[compatibility.scala 127:12]
    Carry_lv2[1] <= _WIRE_50 @[compatibility.scala 127:12]
    Carry_lv2[2] <= _WIRE_51 @[compatibility.scala 127:12]
    Carry_lv2[3] <= _WIRE_52 @[compatibility.scala 127:12]
    Carry_lv2[4] <= _WIRE_53 @[compatibility.scala 127:12]
    Carry_lv2[5] <= _WIRE_54 @[compatibility.scala 127:12]
    Carry_lv2[6] <= _WIRE_55 @[compatibility.scala 127:12]
    Carry_lv2[7] <= _WIRE_56 @[compatibility.scala 127:12]
    Carry_lv2[8] <= _WIRE_57 @[compatibility.scala 127:12]
    Carry_lv2[9] <= _WIRE_58 @[compatibility.scala 127:12]
    Carry_lv2[10] <= _WIRE_59 @[compatibility.scala 127:12]
    Carry_lv2[11] <= _WIRE_60 @[compatibility.scala 127:12]
    Carry_lv2[12] <= _WIRE_61 @[compatibility.scala 127:12]
    Carry_lv2[13] <= _WIRE_62 @[compatibility.scala 127:12]
    Carry_lv2[14] <= _WIRE_63 @[compatibility.scala 127:12]
    Carry_lv2[15] <= _WIRE_64 @[compatibility.scala 127:12]
    Carry_lv2[16] <= _WIRE_65 @[compatibility.scala 127:12]
    node _T = bits(io.in, 0, 0) @[test.scala 92:25]
    CSA_UP[0].a <= _T @[test.scala 92:17]
    node _T_1 = bits(io.in, 1, 1) @[test.scala 93:25]
    CSA_UP[0].b <= _T_1 @[test.scala 93:17]
    CSA_UP[0].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[0].b <= UInt<1>("h00") @[test.scala 95:17]
    Sum[1] <= CSA_UP[0].sum @[test.scala 97:22]
    Carry[1] <= CSA_UP[0].cout @[test.scala 98:24]
    node _T_2 = bits(io.in, 1, 1) @[test.scala 92:25]
    CSA_UP[1].a <= _T_2 @[test.scala 92:17]
    node _T_3 = bits(io.in, 2, 2) @[test.scala 93:25]
    CSA_UP[1].b <= _T_3 @[test.scala 93:17]
    CSA_UP[1].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[1].b <= UInt<1>("h00") @[test.scala 95:17]
    node _T_4 = bits(io.in, 2, 2) @[test.scala 92:25]
    CSA_UP[2].a <= _T_4 @[test.scala 92:17]
    node _T_5 = bits(io.in, 3, 3) @[test.scala 93:25]
    CSA_UP[2].b <= _T_5 @[test.scala 93:17]
    CSA_UP[2].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[2].b <= UInt<1>("h00") @[test.scala 95:17]
    Sum[5] <= CSA_UP[2].sum @[test.scala 97:22]
    Carry[5] <= CSA_UP[2].cout @[test.scala 98:24]
    node _T_6 = bits(io.in, 3, 3) @[test.scala 92:25]
    CSA_UP[3].a <= _T_6 @[test.scala 92:17]
    node _T_7 = bits(io.in, 4, 4) @[test.scala 93:25]
    CSA_UP[3].b <= _T_7 @[test.scala 93:17]
    CSA_UP[3].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[3].b <= UInt<1>("h00") @[test.scala 95:17]
    node _T_8 = bits(io.in, 4, 4) @[test.scala 92:25]
    CSA_UP[4].a <= _T_8 @[test.scala 92:17]
    node _T_9 = bits(io.in, 5, 5) @[test.scala 93:25]
    CSA_UP[4].b <= _T_9 @[test.scala 93:17]
    CSA_UP[4].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[4].b <= UInt<1>("h00") @[test.scala 95:17]
    Sum[9] <= CSA_UP[4].sum @[test.scala 97:22]
    Carry[9] <= CSA_UP[4].cout @[test.scala 98:24]
    node _T_10 = bits(io.in, 5, 5) @[test.scala 92:25]
    CSA_UP[5].a <= _T_10 @[test.scala 92:17]
    node _T_11 = bits(io.in, 6, 6) @[test.scala 93:25]
    CSA_UP[5].b <= _T_11 @[test.scala 93:17]
    CSA_UP[5].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[5].b <= UInt<1>("h00") @[test.scala 95:17]
    node _T_12 = bits(io.in, 6, 6) @[test.scala 92:25]
    CSA_UP[6].a <= _T_12 @[test.scala 92:17]
    node _T_13 = bits(io.in, 7, 7) @[test.scala 93:25]
    CSA_UP[6].b <= _T_13 @[test.scala 93:17]
    CSA_UP[6].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[6].b <= UInt<1>("h00") @[test.scala 95:17]
    Sum[13] <= CSA_UP[6].sum @[test.scala 97:22]
    Carry[13] <= CSA_UP[6].cout @[test.scala 98:24]
    node _T_14 = bits(io.in, 7, 7) @[test.scala 92:25]
    CSA_UP[7].a <= _T_14 @[test.scala 92:17]
    node _T_15 = bits(io.in, 8, 8) @[test.scala 93:25]
    CSA_UP[7].b <= _T_15 @[test.scala 93:17]
    CSA_UP[7].b <= UInt<1>("h00") @[test.scala 94:17]
    CSA_UP[7].b <= UInt<1>("h00") @[test.scala 95:17]
    CSA_UP[8].a <= CSA_UP[0].sum @[test.scala 108:31]
    CSA_UP[8].b <= CSA_UP[0].cout @[test.scala 109:31]
    CSA_UP[8].c <= CSA_UP[1].sum @[test.scala 110:31]
    CSA_UP[8].d <= CSA_UP[1].sum @[test.scala 111:31]
    Sum[3] <= CSA_UP[8].sum @[test.scala 115:19]
    Carry[3] <= CSA_UP[8].cout @[test.scala 116:21]
    CSA_UP[9].a <= CSA_UP[2].sum @[test.scala 108:31]
    CSA_UP[9].b <= CSA_UP[2].cout @[test.scala 109:31]
    CSA_UP[9].c <= CSA_UP[3].sum @[test.scala 110:31]
    CSA_UP[9].d <= CSA_UP[3].sum @[test.scala 111:31]
    CSA_UP[10].a <= CSA_UP[4].sum @[test.scala 108:31]
    CSA_UP[10].b <= CSA_UP[4].cout @[test.scala 109:31]
    CSA_UP[10].c <= CSA_UP[5].sum @[test.scala 110:31]
    CSA_UP[10].d <= CSA_UP[5].sum @[test.scala 111:31]
    Sum[11] <= CSA_UP[10].sum @[test.scala 115:19]
    Carry[11] <= CSA_UP[10].cout @[test.scala 116:21]
    CSA_UP[11].a <= CSA_UP[6].sum @[test.scala 108:31]
    CSA_UP[11].b <= CSA_UP[6].cout @[test.scala 109:31]
    CSA_UP[11].c <= CSA_UP[7].sum @[test.scala 110:31]
    CSA_UP[11].d <= CSA_UP[7].sum @[test.scala 111:31]
    CSA_UP[12].a <= CSA_UP[8].sum @[test.scala 108:31]
    CSA_UP[12].b <= CSA_UP[8].cout @[test.scala 109:31]
    CSA_UP[12].c <= CSA_UP[9].sum @[test.scala 110:31]
    CSA_UP[12].d <= CSA_UP[9].sum @[test.scala 111:31]
    Sum[7] <= CSA_UP[12].sum @[test.scala 115:19]
    Carry[7] <= CSA_UP[12].cout @[test.scala 116:21]
    CSA_UP[13].a <= CSA_UP[10].sum @[test.scala 108:31]
    CSA_UP[13].b <= CSA_UP[10].cout @[test.scala 109:31]
    CSA_UP[13].c <= CSA_UP[11].sum @[test.scala 110:31]
    CSA_UP[13].d <= CSA_UP[11].sum @[test.scala 111:31]
    CSA_UP[14].a <= CSA_UP[12].sum @[test.scala 108:31]
    CSA_UP[14].b <= CSA_UP[12].cout @[test.scala 109:31]
    CSA_UP[14].c <= CSA_UP[13].sum @[test.scala 110:31]
    CSA_UP[14].d <= CSA_UP[13].sum @[test.scala 111:31]
    Sum[15] <= CSA_UP[14].sum @[test.scala 115:19]
    Carry[15] <= CSA_UP[14].cout @[test.scala 116:21]
    CSA_DOWN[0].a <= Sum[7] @[test.scala 136:32]
    CSA_DOWN[0].b <= Carry[7] @[test.scala 137:32]
    CSA_DOWN[0].c <= Sum[11] @[test.scala 138:32]
    CSA_DOWN[0].d <= Carry[11] @[test.scala 139:32]
    Sum_lv2[11] <= CSA_DOWN[0].sum @[test.scala 140:33]
    Carry_lv2[11] <= CSA_DOWN[0].cout @[test.scala 141:35]
    CSA_DOWN[1].a <= Sum[3] @[test.scala 136:32]
    CSA_DOWN[1].b <= Carry[3] @[test.scala 137:32]
    CSA_DOWN[1].c <= Sum[5] @[test.scala 138:32]
    CSA_DOWN[1].d <= Carry[5] @[test.scala 139:32]
    Sum_lv2[5] <= CSA_DOWN[1].sum @[test.scala 140:33]
    Carry_lv2[5] <= CSA_DOWN[1].cout @[test.scala 141:35]
    CSA_DOWN[2].a <= Sum[7] @[test.scala 136:32]
    CSA_DOWN[2].b <= Carry[7] @[test.scala 137:32]
    CSA_DOWN[2].c <= Sum[9] @[test.scala 138:32]
    CSA_DOWN[2].d <= Carry[9] @[test.scala 139:32]
    Sum_lv2[9] <= CSA_DOWN[2].sum @[test.scala 140:33]
    Carry_lv2[9] <= CSA_DOWN[2].cout @[test.scala 141:35]
    CSA_DOWN[3].c <= Sum[3] @[test.scala 162:23]
    CSA_DOWN[3].d <= Carry[3] @[test.scala 163:23]
    Sum_lv2[5] <= CSA_DOWN[3].sum @[test.scala 165:26]
    Sum_lv2[5] <= CSA_DOWN[3].cout @[test.scala 166:26]
    CSA_DOWN[4].a <= Sum[1] @[test.scala 136:32]
    CSA_DOWN[4].b <= Carry[1] @[test.scala 137:32]
    CSA_DOWN[4].c <= Sum[2] @[test.scala 138:32]
    CSA_DOWN[4].d <= Carry[2] @[test.scala 139:32]
    Sum_lv2[2] <= CSA_DOWN[4].sum @[test.scala 140:33]
    Carry_lv2[2] <= CSA_DOWN[4].cout @[test.scala 141:35]
    CSA_DOWN[5].a <= Sum[3] @[test.scala 136:32]
    CSA_DOWN[5].b <= Carry[3] @[test.scala 137:32]
    CSA_DOWN[5].c <= Sum[4] @[test.scala 138:32]
    CSA_DOWN[5].d <= Carry[4] @[test.scala 139:32]
    Sum_lv2[4] <= CSA_DOWN[5].sum @[test.scala 140:33]
    Carry_lv2[4] <= CSA_DOWN[5].cout @[test.scala 141:35]
    CSA_DOWN[6].a <= Sum[5] @[test.scala 136:32]
    CSA_DOWN[6].b <= Carry[5] @[test.scala 137:32]
    CSA_DOWN[6].c <= Sum[6] @[test.scala 138:32]
    CSA_DOWN[6].d <= Carry[6] @[test.scala 139:32]
    Sum_lv2[6] <= CSA_DOWN[6].sum @[test.scala 140:33]
    Carry_lv2[6] <= CSA_DOWN[6].cout @[test.scala 141:35]
    CSA_DOWN[7].a <= Sum[7] @[test.scala 136:32]
    CSA_DOWN[7].b <= Carry[7] @[test.scala 137:32]
    CSA_DOWN[7].c <= Sum[8] @[test.scala 138:32]
    CSA_DOWN[7].d <= Carry[8] @[test.scala 139:32]
    Sum_lv2[8] <= CSA_DOWN[7].sum @[test.scala 140:33]
    Carry_lv2[8] <= CSA_DOWN[7].cout @[test.scala 141:35]
    node _T_16 = bits(io.in, 8, 8) @[test.scala 159:31]
    CSA_DOWN[8].c <= _T_16 @[test.scala 159:23]
    CSA_DOWN[8].d <= UInt<1>("h00") @[test.scala 160:23]
    Sum_lv2[2] <= CSA_DOWN[8].sum @[test.scala 165:26]
    Sum_lv2[2] <= CSA_DOWN[8].cout @[test.scala 166:26]
    CSA_DOWN[9].c <= Sum[9] @[test.scala 162:23]
    CSA_DOWN[9].d <= Carry[9] @[test.scala 163:23]
    Sum_lv2[2] <= CSA_DOWN[9].sum @[test.scala 165:26]
    Sum_lv2[2] <= CSA_DOWN[9].cout @[test.scala 166:26]
    node _T_17 = bits(io.in, 10, 10) @[test.scala 159:31]
    CSA_DOWN[10].c <= _T_17 @[test.scala 159:23]
    CSA_DOWN[10].d <= UInt<1>("h00") @[test.scala 160:23]
    Sum_lv2[2] <= CSA_DOWN[10].sum @[test.scala 165:26]
    Sum_lv2[2] <= CSA_DOWN[10].cout @[test.scala 166:26]
    
