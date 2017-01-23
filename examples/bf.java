// A simple Brainfuck program to compute 3*5 and obtain 15.

class BF {
    int compute_3x5 = run_bf(
        // put 3 in cell 0
        PLUS ^ PLUS ^ PLUS
        // put 5 in cell 1
      ^ RIGHT ^ PLUS ^ PLUS ^ PLUS ^ PLUS ^ PLUS
     
        // while cell 0 is not empty
      ^ LEFT
      ^ LOOP(
          // decrement cell 0
          MINUS
          // add cell 1 to cells 2 and 3
        ^ RIGHT
        ^ LOOP(
              MINUS
            ^ RIGHT ^ PLUS
            ^ RIGHT ^ PLUS
            ^ LEFT ^ LEFT
          )
          // move cell 3 back to cell 1
        ^ RIGHT ^ RIGHT
        ^ LOOP(
              MINUS
            ^ LEFT ^ LEFT
            ^ PLUS
            ^ RIGHT ^ RIGHT
          )
        ^ LEFT ^ LEFT ^ LEFT
        )
      ^ RIGHT ^ RIGHT // cell 2 contains the result (15)
      ^ nil);
}
