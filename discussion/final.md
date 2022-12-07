<bhr>Terminal Presentation</bhr>
> 2022 FALL CSE 230
> Group 8 Project Presentation
> 2022.12.07
===
<bhr>Members  +  Contributions</bhr>
---
![q](quan.png)
## Quan Luo
Quan mainly worked on Brick library related implementations
and image library. He also built the architecture
and designed the interfaces.
---
![i](issei.png)
# Issei Mori
Issei mainly worked on the parser part
that is to parse a MarkDown file
into data structures our program can understand.
---
![z](z.png)
#### Z (Ziang) Xiao
Z mainly worked on the visual layout and style part.
He also takes the team logistics and coordination.
===
<bhr>TerminalMD</bhr>
---
<bhr>Overview</bhr>
---
# What does it do?
- Take a Markdown file from the user     
- Parse into predefined interfaces       
- Visualize the content in terminal window
---
### Example usage
- Install the app with stack   
    - `stack build`            
- Input a Markdown file and run
    - `stack run simple.md`    
---
#### Features
- Interactive TUI                  
- Markdown syntax                  
- Page control                     
- Beautiful layout and design      
- Terminal preview of Markdown file
- **A cool tool to present!          **
===
 
<bhr>Architecture</bhr>
 
---
 
<bhr>Parser  +  Interface</bhr>
 
---
 
(Issei, 1 min)
===
 
<bhr>Architecture</bhr>
 
---
 
<bhr>Visualizer</bhr>
 
---
 
# Brick Interface
`data AppState {`
`    slides,    `
`    curIndex,  `
`    images     `
`}              `

- `appDraw` - draw elements top to down
- `handleEvent` - switch ppt page      
 
---
 
## Visualizing One Slide
- `visualizeBlock` - care for block layout    
- `visualizeMD` - care for markdown type      
- `visualizeText` - care for specific elements
===
 
<bhr>Architecture</bhr>
 
---
 
<bhr>Unit  Test  +  Development</bhr>
 
---
 
### Unit Test
- `Test.tasty` framework for testing  
- Multiple unittest added during dev
- Push hook before pushing to remote
 
---
 
#### Development
- Fixed interface at the first meeting
- Independent development             
    - parser                          
    - visualizer                      
===
 
<bhr>Demo</bhr>
 
(Issei, 1 min)
Present all the md syntax we support (Left: md, Right: vis)
===
 
<bhr>Challenges</bhr>
 
---
 
<bhr>Parser</bhr>
 
---
 
(Issei, 30s)
===
 
<bhr>Challenges</bhr>
 
---
 
<bhr>Visualizer</bhr>
 
---
 
#### Manipulation on Brick `widgets`
- Alignment                           
   - Left align vs. center align         
- Layout                              
   - Element as minimal visualized unit   
   - Element encapsulated by content block
       - Differentiate for each Markdown type
       - Differentiate according to context  
 
### Styling is very interesting!
 
Colors and emojis are fun.
 
🍉  🐸  🐤  💎
 
===
 
<bhr>Challenges</bhr>
 
---
 
<bhr>Images</bhr>
 
---
 
### How to read image
- JuicyPixel package `readImage`        
- Read image in `initialState`          
    - IO cannot be done in non-IO func
    - Reading images is slow          
## How to set AttrMap
- AttrMap is static but color space is 256 x 256 x 256
- Discretize color to 8 x 8 x 8                       

===
<bhr> Questions  +  Answers</bhr>
===
<bhr>Thank  you!</bhr>
 
---
 
![i](nadia.png)


