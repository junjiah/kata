package boolfuck

import (
	"errors"
	"fmt"
	"strconv"
)

// Valid commands.
var commands map[rune]struct{}

func init() {
	commands = make(map[rune]struct{})
	for _, ch := range "+,;<>[]" {
		commands[ch] = struct{}{}
	}
}

func parseCode(code string) (cleanCode string, jumpTable map[int]int, err error) {
	var parsed []rune
	var matchingBracketIdxStack []int

	jumpTable = make(map[int]int)

	idx := 0 // Index only for parsed code.
	for _, ch := range code {
		if _, ok := commands[ch]; !ok {
			continue
		}
		parsed = append(parsed, ch)

		if ch == '[' {
			matchingBracketIdxStack = append(matchingBracketIdxStack, idx)
		} else if ch == ']' {
			if len(matchingBracketIdxStack) == 0 {
				err = errors.New("Unmatching brackets")
				return
			}
			l := len(matchingBracketIdxStack)
			matched := matchingBracketIdxStack[l-1]
			matchingBracketIdxStack = matchingBracketIdxStack[:l-1]
			jumpTable[idx] = matched
			jumpTable[matched] = idx
		}

		idx++
	}
	cleanCode = string(parsed)
	return
}

// Helper func to convert bit representation (in bytes) to little-endian string.
func parseBitsToString(bits []byte) string {
	// Padding 0.
	for len(bits)%8 != 0 {
		bits = append(bits, 0)
	}

	var outputChars []byte
	byteAsBits := make([]byte, 8)
	for i := 0; i < len(bits); i += 8 {
		// Reverse bits for little endian representation.
		for j := 0; j < 8; j++ {
			byteAsBits[j] = bits[i+8-j-1] + byte('0')
		}
		ch, _ := strconv.ParseInt(string(byteAsBits), 2, 64)
		outputChars = append(outputChars, byte(ch))
	}
	return string(outputChars)
}

// Helper func to convert string to bit representation (in bytes) of little-endian format.
func parseStringToBits(input string) []byte {
	var res []byte
	// NOTE: important not to iterate by runes.
	for i := range input {
		s := fmt.Sprintf("%08b", input[i])
		// Reverse for little-endian format.
		for i := 7; i >= 0; i-- {
			res = append(res, byte(s[i])-'0')
		}
	}
	return res
}

// Boolfuck is the interpreter for boolfuck esolang.
func Boolfuck(rawCode, input string) string {
	code, jumpTable, err := parseCode(rawCode)
	if err != nil {
		panic(fmt.Sprintf("Invalid input: %s", err.Error()))
	}

	inputBits := parseStringToBits(input)
	outputBits := []byte{}
	mem := map[int]byte{}
	cmdP, dataP := 0, 0

	for cmdP < len(code) {
		switch code[cmdP] {
		case '+':
			mem[dataP] ^= 0x1
		case '<':
			dataP--
		case '>':
			dataP++
		case '[':
			if mem[dataP] == 0 {
				cmdP = jumpTable[cmdP]
			}
		case ']':
			cmdP = jumpTable[cmdP] - 1 // Which will increment later.
		case ';':
			outputBits = append(outputBits, mem[dataP])
		case ',':
			if len(inputBits) == 0 {
				mem[dataP] = 0
			} else {
				mem[dataP] = inputBits[0]
				inputBits = inputBits[1:]
			}
		}
		cmdP++
	}
	return parseBitsToString(outputBits)
}
