# day01.ua
TestF ← "data/day01-test.txt"
TestTwoF ← "data/day01-test2.txt"
InputF ← "data/day01-input.txt"

SplitLines ← ⊜□≠, @\n
KeepDigits ← -48 ▽ ×⊃(≤57)(≥48).-@\0
MakeNumber ← /+ ×[10 1] ⊂ ⊃⊢(⊢ ⇌)
PartOne ← /+ ∵(MakeNumber KeepDigits) SplitLines &fras

⍤ .= 142 PartOne TestF
PartOne InputF
