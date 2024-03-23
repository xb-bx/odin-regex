package regex
import "core:fmt"
import "core:strings"
import "core:unicode/utf8"
RegularChar :: distinct rune
EOL :: struct {}
StartOfLine :: struct {}
ZeroOrMore :: struct {}
OneOrMore :: struct {}
End :: struct {}
AnyChar :: struct {}
Maybe :: struct {}
Range :: struct {
	start: int,
	end:   int,
}
Ranges :: struct {
	ranges:   []Range,
	inverted: bool,
}
RegexNode :: union {
	AnyChar,
	RegularChar,
	EOL,
	StartOfLine,
	ZeroOrMore,
	OneOrMore,
	Maybe,
	End,
	Ranges,
}


Regex :: struct {
	nodes: []RegexNode,
}
Stack :: struct($TElem: typeid) {
	items: [dynamic]TElem,
}
make_stack :: proc($T: typeid) -> Stack(T) {
	return Stack(T){items = make([dynamic]T)}
}
delete_stack :: proc(stack: Stack($T)) {
	delete(stack.items)
}
stack_push :: proc(stack: ^Stack($T), elem: T) {
	append(&stack.items, elem)
}
stack_peek :: proc(stack: ^Stack($T)) -> ^T {
	if len(stack.items) <= 0 do panic("empty stack")
	res := &stack.items[len(stack.items) - 1]
	return res
}
stack_pop :: proc(stack: ^Stack($T)) -> T {
	if len(stack.items) <= 0 do panic("empty stack")
	res := stack.items[len(stack.items) - 1]
	ordered_remove(&stack.items, len(stack.items) - 1)
	return res
}
RegexState :: struct {
	str:             string,
	nodes:           []RegexNode,
	nodei:           int,
	offset:          int,
	backtrack_stack: ^Stack(RegexState),
	result:          bool,
	start_offset:    int,
	is_start:        bool,
}

regex_step :: proc(using regex_state: ^RegexState) -> (res: bool) {
	if _, ok := nodes[nodei].(End); ok {
		result = true
		return false
	}
	if _, ok := nodes[nodei].(EOL); ok {
        if offset >= len(str) {
            if start_offset == -1 do start_offset = offset
            result = true
            return false
        }
        return false
	}
	if offset > len(str) do return false
	run, size := utf8.decode_rune(str[offset:])
	if size <= 0 do return false
	#partial switch node in nodes[nodei] {
	case End:
		result = true
		return false
	case StartOfLine:
		if offset == 0 && is_start {
            if start_offset == -1 do start_offset = offset
			nodei += 1
			return true
		}
		return false
	case RegularChar:
		if rune(node) == run {
			if start_offset == -1 do start_offset = offset
			nodei += 1
			offset += size
			return true
		} else {
			offset += size
			nodei = 0
			return false
		}
	case Ranges:
		if node.inverted {
			for range in node.ranges {
				if int(run) >= range.start && int(run) <= range.end {
					offset += size
					nodei = 0
					return false
				}
			}
			if start_offset == -1 do start_offset = offset
			nodei += 1
			offset += size
			return true

		} else {
			for range in node.ranges {
				if int(run) >= range.start && int(run) <= range.end {
					if start_offset == -1 do start_offset = offset
					offset += size
					nodei += 1
					return true
				}
			}
			nodei = 0
			offset += size
			return false
		}
	case OneOrMore:
		new_state := RegexState {
			str             = str[offset:],
			nodes           = nodes[nodei:],
			backtrack_stack = backtrack_stack,
			start_offset    = start_offset,
		}
		nodei += 2
		new_state.nodei = 1
		if !regex_step(&new_state) do return false
		if start_offset == -1 do start_offset = offset
		new_state.nodei = 1
		orig_offset := regex_state.offset
        offset += new_state.offset
		stack_push(backtrack_stack, regex_state^)
		for regex_step(&new_state) {
			new_state.nodei = 1
			offset = orig_offset + new_state.offset
			stack_push(backtrack_stack, regex_state^)
		}
		stack_pop(backtrack_stack)
		return true
	case ZeroOrMore:
		new_state := RegexState {
			str             = str[offset:],
			nodes           = nodes[nodei:],
			backtrack_stack = backtrack_stack,
			start_offset    = start_offset,
		}
		if start_offset == -1 do start_offset = offset
		nodei += 2
		new_state.nodei = 1
		stack_push(backtrack_stack, regex_state^)
		orig_start := start_offset
		orig_offset := regex_state.offset
		for regex_step(&new_state) {
			im_the_start := start_offset == -1
			if im_the_start do start_offset = offset
			new_state.nodei = 1
			offset = orig_offset + new_state.offset
			stack_push(backtrack_stack, regex_state^)
			if im_the_start do stack_peek(backtrack_stack).start_offset = orig_start
		}
		stack_pop(backtrack_stack)
		return true
	case Maybe:
		if start_offset == -1 do start_offset = offset
		new_state := RegexState {
			str             = str[offset:],
			nodes           = nodes[nodei:],
			backtrack_stack = backtrack_stack,
			start_offset    = start_offset,
		}
		nodei += 2
		new_state.nodei = 1
		stack_push(backtrack_stack, regex_state^)
		if regex_step(&new_state) {
			offset += new_state.offset
		}
		return true
	case AnyChar:
		nodei += 1
		offset += size
		return true
	case:
		if true do panic("")
	}
	return false
}
MatchResult :: struct {
	start: int,
	end:   int,
}
match_regex :: proc(regex: Regex, str: string, is_start_of_line: bool = true) -> (match: MatchResult, res: bool) {
	state := RegexState {
		str             = str,
		nodes           = regex.nodes,
		backtrack_stack = new_clone(make_stack(RegexState)),
		start_offset    = -1,
		is_start        = is_start_of_line,
	}
	offset := 0
	for !state.result && offset < len(str) {
		state.result = false
		state.nodei = 0
		for regex_step(&state) {
		}
		if state.result {
			clear(&state.backtrack_stack.items)
			break
		}
		for len(state.backtrack_stack.items) > 0 {
			newstate := stack_pop(state.backtrack_stack)
            newstate.backtrack_stack = new_clone(make_stack(RegexState))
			for regex_step(&newstate) {}
			if newstate.result {
				state = newstate
				break
			}

		}
		if state.result {
			clear(&state.backtrack_stack.items)
			break
		}
		offset += 1
        state.is_start = false
		state.offset = 0
		state.str = str[offset:]
		state.start_offset = -1
		clear(&state.backtrack_stack.items)
	}
	delete_stack(state.backtrack_stack^)
	free(state.backtrack_stack)
	return MatchResult{state.start_offset + offset, state.offset + offset}, state.result
}
MatcherState :: struct {
	regex: Regex,
	str:   string,
	off:   int,
}
regex_iter_matches :: proc(using matcher_state: ^MatcherState) -> (match: MatchResult, res: bool) {
	if off > len(str) do return {}, false
	if match, ok := match_regex(regex, str[off:], off == 0); ok {
		res := MatchResult{match.start + off, match.end + off}
		off += match.start == match.end ? 1 : match.end
		return res, true
	}
	return {}, false
}
compile_regex :: proc(regex_str: string) -> (regex: Regex, ok: bool) {
	get_inside :: proc(str: string) -> (string, int) {
		i := 0
		for i < len(str) {
			c, size := utf8.decode_rune(str[i:])
			if size <= 0 do return "", -1
			switch c {
			case ']':
				return str[:i], i + 1
			case '\\':
				i += size
				if i >= len(str) do return "", -1
				c, size := utf8.decode_rune(str[i:])
				if size <= 0 do return "", -1
				i += size
                
			case:
				i += size
			}
		}
		return "", -1
	}
	nodes := make([dynamic]RegexNode)
	defer if !ok do delete(nodes)
	i := 0

	for i < len(regex_str) {
		c, size := utf8.decode_rune(regex_str[i:])
		if size <= 0 do return {}, false
		i += size
		switch c {
		case '+':
			if len(nodes) == 0 do return {}, false
			inject_at(&nodes, len(nodes) - 1, OneOrMore{})
		case '*':
			if len(nodes) == 0 do return {}, false
			inject_at(&nodes, len(nodes) - 1, ZeroOrMore{})
		case '?':
			if len(nodes) == 0 do return {}, false
			inject_at(&nodes, len(nodes) - 1, Maybe{})
		case '.':
			append(&nodes, AnyChar{})
        case '^':
			append(&nodes, StartOfLine{})
        case '$':
			append(&nodes, EOL{})

		case '[':
			ranges := make([dynamic]Range)
			if i >= len(regex_str) do return {}, false
			inside, closing_off := get_inside(regex_str[i:])
			if closing_off == -1 do return {}, false
			len := closing_off - 1
			i += closing_off
			ri := 0
			inverted := len > 0 && inside[0] == '^'
			if inverted do ri += 1
			for ri < len {
				run := utf8.rune_at(inside, ri)
				run_size := utf8.rune_size(run)
				if run == '-' do return {}, false
				else if ri + run_size < len && utf8.rune_at(inside, ri + run_size) == '-' {
					ri += run_size + 1
					if ri >= len do return {}, false
					start := int(run)
					run, run_size = utf8.decode_rune(inside[ri:])
					append(&ranges, Range{start = start, end = int(run)})
					ri += run_size
				} else {
					append(&ranges, Range{start = int(run), end = int(run)})
					ri += run_size
				}
			}
			append(&nodes, Ranges{ranges = ranges[:], inverted = inverted})

		case '\\':
			nextchar, size := utf8.decode_rune(regex_str[i:])
			if size <= 0 do return {}, false
			i += size
			append(&nodes, RegularChar(nextchar))

		case:
			append(&nodes, RegularChar(c))
		}

	}
	append(&nodes, End{})
	return Regex{nodes = nodes[:]}, true
}
delete_regex :: proc(regex: Regex) {
    for node in regex.nodes {
        if ranges, ok :=  node.(Ranges); ok {
            delete(ranges.ranges)
        }
    }
	delete(regex.nodes)
}
matches :: proc(regex: Regex, str: string) {
	state := MatcherState {
		regex = regex,
		str   = str,
		off   = 0,
	}
	for match in regex_iter_matches(&state) {
		fmt.println("match: ", str[match.start:match.end], match.start, match.end)
	}
}
main :: proc() {
	regex, ok := compile_regex("import \"core:.+\"")
	fmt.println(regex, ok)
	matches(regex, "import \"core:c\"")
}
