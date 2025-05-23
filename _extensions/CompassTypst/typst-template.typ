
#let CompassTypst(

  // Here is where we define our variables
  
  // The document title.
  
  title: "The Report's Title",
  
  // The document subtitle
  
  subtitle: none,
  
  // The document date
  date: datetime.today(),

  // Logo in top right corner.
  typst-logo: none,
  
  // Abstract.
  abstract: none,
  
  // Author
  author: none,
  
  // Color of the footer
  footer_color: "#8CD3D6",
  
  keywords: (),
  
  // Title font size
  
  title_font_size: "38",
  
  // Subtitle font size
  
  subtitle_font_size: "22",


  // The documents content.
  
  
  body
  
  ) = {

  // Font size and others
  
  set text(font: "Nunito", 12pt, weight: 500)
  
  title_font_size = int(title_font_size) * 1pt


  // Configure the pages.
  
  set page(
 
  // margin: (left: 25%),
   
    numbering: "1",
    number-align: center,
    
    header: align(
    right + horizon,
    pad(x: -1.7cm, top: .5cm, image(typst-logo.path, width: 4cm))
    ),
    
  // This sets background color strip on right
  
  background: place(left + top, rect(
      fill: rgb("#5cb1b4"),
      height: 100%,
      width: 1%,
    ))
  )
  
// Configure paragraph indents: hanging-indent and 
//  set par(hanging-indent: 1em)
// Configure lvl-1 indent of numbered lists
  set enum(indent: 1em)
// COnfigure lvl-1 indent of bulleted list
  set list(indent: 1em)
// Configure numbering of headings.
  set heading(numbering: "1.")
  
  show heading: set text(font: "Nunito", 20pt, weight: 800)
  show heading.where(level: 1): set text(1.1em)
  // show heading.where(level: 1): underline
  show heading.where(level: 1): set par(leading: 0.4em)
  show heading.where(level: 1): set block(below: 0.8em)
  show heading.where(level: 2): set block(below: 0.8em)
  

  // Links should be Compass Blue.
  
  show link: set text(rgb("#2fc5a5"))


  // blue border column
  
  grid(
    columns: (1fr),
  
   
    // Title & Subtitle.
    
   pad(bottom: .5cm, text(font: "Ubuntu", 24pt, weight: 800, upper(title))),
   pad(bottom: .5cm, text(font: "Nunito", 14pt, weight: 500, upper(subtitle))),
   
   // Published and Author in two columns.
   
   
    grid(
      columns: (1fr, 1fr),
      pad(bottom: 1cm, text(font: "Nunito", 12pt, weight: 500, "Published: " + date)),
      pad(bottom: 1cm, text(font: "Nunito", 12pt, weight: 500, "Author: " + author)),
    ),

    // The main body text.
    {
      set par(justify: true)
      body
      v(1fr)
    },
  

  )
}

#set table(
  inset: 6pt,
  stroke: none
)

