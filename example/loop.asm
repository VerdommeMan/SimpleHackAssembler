// Sourced from http://www.jk-quantized.com/experiments/HomebrewComputer/Cheatsheets/hackASM.html
// --- Computes Memory[ 1 ] = 1 + 2 + ... + Memory[ 0 ]
// --- Usage: load a value into Memory[ 0 ]

// Setup
@i
M = 1		    // Memory[ @i ] = Memory[ 16 ] = 1  // represents i

@sum
M = 0		    // Memory[ @sum ] = Memory[ 17 ] = 0  // represents sum

// Main
(loop)

	// break condition
	@i
	D = M
	@0
	D = D - M   // i - Memory[ 0 ]
	@loopEnd
	D ; JGT		// if i > Memory[ 0 ], exit loop


	// body
	@i
	D = M
	@sum
	M = D + M 	// sum += i

	@i
	M = M + 1 	// i += 1

	@loop
	0 ; JMP 	// go to next iteration of loop

(loopEnd)

	@sum
	D = M
	@1
	M = D 		// Memory[ 1 ] = sum  // done

// Program end
(end)

	@end
	0 ; JMP 	// loop indefinitely
@
(*

sf:L
tsetst

@ end