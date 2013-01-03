[last updated: 2012-11-01]

Chess4Net DB
-------------

Chess4Net DB is a set of two files <base>.pos and <base>.mov

Chess4Net DB structure
-----------------------

<pos-file structure> ::= [<pos-header>]<pos-data> // if no header then version = 0, otherwise version >= 1
  <pos-header> ::= $FF <pos-header-data>
    <pos-header-data> ::= <version[word]>
  <pos-data> ::= (<pos-node>)*
    <pos-node> ::= <field[byte]><next-node[byte+word]><next-value[byte+word]>

<mov-file structure> ::= (<mov-node>)*
  <mov-node> ::= <move[word]><estimate[dword]><next-value[byte+word]>


Switches explanation:
----------------------

PosDB [-V] [-E -U] [-P <name>] [-W|B] [-C <number of plys>] [-O|-X] [-R <referenced base>] <input PGN file> [<base>]

-V	proceed also variants.
-E	change estimation for moves.
-U	include only unique positions per game into the move estimation.
-P	proceed only positions played by the player <name>.
-W	proceed only positions played by white.
-B	proceed only positions played by black.
-C	include compulsory <number of plys> plys into the base.
-O	generate only opening lines.
-X	generate extended opening lines.
-X+ generate extended opening lines with simple positions.
-R	use <referenced base> as a base for references.
-T  build move tree DB.

PGN Example:

[White "Chigorin, Mikhail"]
[Black "Schiffers, Emanuel Stepanovich"]

1. e4 e5 2. Nf3 Nc6 3. d4 exd4 4. Nxd4 Bc5 5. Be3 Qf6 6. c3 Nge7 7. Bc4 Ne5 8.
Be2 d5 9. f4 Ng4 10. Bxg4 Qh4+ 11. g3 Qxg4 12. Qxg4 Bxg4 13. e5 O-O 14. Nf5
Bxe3 15. Nxe3 (15. Nxe7+ $2 Kh8 16. Nxd5 Bf3)

-V - (15. Nxe7+ $2 Kh8 16. Nxd5 Bf3) will be also considered

-W - 1. e4 ... 2. Nf3 ... 3. d4 ... etc. are only considered
-B - 1. ... e5 2. ... Nc6 3. ... exd4 etc. are only considered

-E - estimation for every <position, move> is added. It is equal amount of them all over PGN data that is being analyzed.
-U - only unique <position, move> are considered per game.

-P "Chigorin, Mikhail" - 1. e4 ... 2. Nf3 ... 3. d4 ... etc. will be considered
-P "Schiffers, Emanuel Stepanovich" - 1. ... e5 2. ... Nc6 3. ... exd4 etc. will be considered

-C 4 - 1. e4 e5 2. Nf3 Nc6 will be considered only

-R - use a reference base in -O or -X generation mode. If no reference base is given the generated base is used as reference.

-O - use normal opening lines generation.
-X - use extended opening lines generation.
-X+ - use extended opening lines generation. Also simple positions are added at the end of lines (works only in main lines).
-T - also a move tree base will be generated. Data in the generated base is linked with tree nodes in move tree base.

  Consider 1. e4[2] e5[2] 2. Nf3[1] Nc6[2] 3. d4[1] be the reference base (in brackets amount of times the position comes in the base is indicated).
  Let 1. e4 Nc6 2. Nf3 e5 3. d4 be used for base generation.
  -O -R will produce 1. e4 Nc6 only
  -X -R will produce 1. e4 Nc6 2. Nf3 e5 since e5 has [2] in the reference base Nf3 is added as a linking move too.
  -X+ -R will produce 1. e4 Nc6 2. Nf3 e5 3. d4 since d4 will be appended as a simple position.

MoveTree Algorithm
-------------------

f.e:

I.   1. e4 e5 2. Nf3
II.  1. e4 d5
III. 1. e4 e5 2. Bc4
IV.  1. e4 e6
V.   1. d4

produces links (far pointed):

I.
     e2e4    e7e5     g1f3     0   0

II.
     e2e4    $40  $00 $00  [A] 0   0
  A:                                   e7e5 g1f3   d7d5    0   0

III.
     e2e4    $40  $00 $00  [A] $00 [B]
  A:                                   e7e5 $40 00 d7d5    0   0
  B:                                                               g1f3 0 0 f1c4 0 0

IV.
     e2e4    $40  $00 $00  [A] $00 [B]
  A:                                   e7e5 $40 00 $40 $00 $00 [C]
  B:                                                               g1f3 0 0 f1c4 0 0
  C:                                                                                 d7d5 0 0 e7e6 0 0

IV.
     $40 $00 $00  [D] $00  [A] $00 [B]
  A:                                   e7e5 $40 00 $40 $00 $00 [C]
  B:                                                               g1f3 0 0 f1c4 0 0
  C:                                                                                 d7d5 0 0 e7e6 0 0
  D:                                                                                                   e2e4 $40 $00 d2d4 0 0