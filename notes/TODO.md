# TODO

- [ ] expandable/collapsable table of contents
- [ ] tool that converts a parsed Markdown document into my Doc format (for the sake of drafting)
- [x] remove preprocessing step -- the default should just be that a user-defined doc is an `M Aff Doc`, and then any computation necessary happens inside there
- [x] decide on how to semantically categorize structures (e.g. go down to paragraphs and sentences? or just inline-styled groups of strings)
  - the factors here are that making things sentences allows me to do conscious formatting better, but making things just inline-styled groups (for this example) is more generic and makes it so that i dont have to change my mind later about how to define the fundamental data type
- [x] how to handle sidenotes
  - i want something basically like footnotes, but that are accessible inline
    - on desktop, these could appear to the side of the main content; on mobile these could be tapped in order to be expanded
