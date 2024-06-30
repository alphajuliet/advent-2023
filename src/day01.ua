# day01.ua
TestF ← "data/day01-test.txt"
TestTwoF ← "data/day01-test2.txt"
InputF ← "data/day01-input.txt"

SplitLines ← ⊜□≠, @\n
KeepDigits ← ▽×⊃(≤57)(≥48).-@\0
MakeNumber ← /+ ×[10 1] ⊂ ⊃⊢(⊢⇌)
PartOne ← °□ /+ ⍚(MakeNumber -48 KeepDigits) SplitLines

⍤.= 142 PartOne &fras TestF
# PartOne &fras InputF

Numbers ← {
  "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
  "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
}
Convert ← ⨬(+1|-9) >9.
# FindAll ← ≡⊚ ▽⊙⍉ /+. ∵◇⌕ # FindAll candidates target
# PartTwo ← /+ ∵(MakeNumber ♭ ≡Convert FindAll Numbers) SplitLines

# PartTwo is broken: the table find below is not working
# PartTwo &fras TestTwoF

&fras TestTwoF
SplitLines
Numbers.
⊞(⍚⌕)
# ◇∵⍚▽.

[{[1 0 1] [0 1]}
 {[0 0 1] [1 1]}]

◇∵⍚▽.
