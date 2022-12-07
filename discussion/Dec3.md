<bhr>This is a Big Header!</bhr>
# This is Header1
## This is Header2
### This is Header3
#### This is Header4
##### This is Header4+ with some **bold text**
\# this is not a header

**This is a bold text**
<u>This is an underline text</u>

I'm `codes` inlining

> This is a quote!

---
<bhr>Nested Lists</bhr>

for lists like
- I'm list 1
    - I'm list 2

Our parser will result in two markdown types
`[ListBullet 1 [TextStyle], List Bullet 2 [TextStyle]]`

However it's more natural to parse into one markdown type like
`ListBullet [(1, [TextStyle]), (2, [TextStyle]) ...]`

Seemingly for enumerates & codeblocks

> WHY: We need to visualize them as a whole block
---

<bhr>New Features</bhr>

1. Image - parser for `![some_name](filename)`, only need filename for now
2. Layout - presentation needs different layout

Specify layout with ---layout_name
Specify blocks by something like <block></block>

---

<block>
slide element
</block>

<block>
another slide
</block>

--- layout:2col
