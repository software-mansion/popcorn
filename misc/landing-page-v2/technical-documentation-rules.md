# Technical Documentation Rules

A comprehensive style guide for writing clear, consistent, and user-focused technical documentation, based on Google Developer Documentation Style Guide, MDN Writing Style Guide, and Diátaxis framework principles.

## Table of Contents

1. [Core Principles](#core-principles)
2. [Documentation Types (Diátaxis Framework)](#documentation-types-diataxis-framework)
3. [Writing Style](#writing-style)
4. [Language and Grammar](#language-and-grammar)
5. [Structure and Organization](#structure-and-organization)
6. [Code Examples](#code-examples)
7. [Formatting Guidelines](#formatting-guidelines)
8. [Accessibility and Inclusivity](#accessibility-and-inclusivity)
9. [SEO and Discoverability](#seo-and-discoverability)
10. [Review and Quality Assurance](#review-and-quality-assurance)

## Core Principles

### The Three Cs of Good Writing
- **Clear**: Use simple, unambiguous language. Write short sentences with one idea per sentence.
- **Concise**: Provide necessary detail without being verbose. Avoid excessive information that makes content tedious.
- **Consistent**: Use the same terminology, formatting, and style throughout all documentation.

### User-Centered Approach
- Consider your target audience's knowledge level and needs
- Focus on what users want to accomplish, not just what the product does
- Provide context for why information matters to the user
- Include relevant examples and real-world scenarios

### Break the Rules When Necessary
Guidelines are not absolute rules. Depart from these guidelines when doing so improves your content, but be consistent throughout your document.

## Documentation Types (Diátaxis Framework)

Organize documentation into four distinct types based on user needs:

### 1. Tutorials (Learning-Oriented)
**Purpose**: Help users learn through hands-on experience
**Characteristics**:
- Step-by-step lessons for beginners
- Learning-oriented, not goal-oriented
- Show users how to get started
- Build confidence through small wins
- Focus on the journey, not just the destination

**Writing Guidelines**:
- Use second person ("you will learn")
- Include every step, even obvious ones
- Test instructions with beginners
- Provide expected outcomes at each step
- Keep scope narrow and focused

### 2. How-to Guides (Problem-Oriented)
**Purpose**: Guide users through specific tasks
**Characteristics**:
- Practical, goal-oriented directions
- Assume some prior knowledge
- Focus on solving specific problems
- Provide flexible approaches when possible

**Writing Guidelines**:
- Start with the goal or problem
- Use imperative mood ("Click the button")
- Focus on what, not why
- Keep steps minimal but complete
- Provide troubleshooting for common issues

### 3. Reference (Information-Oriented)
**Purpose**: Provide comprehensive technical information
**Characteristics**:
- Complete, accurate technical descriptions
- Organized for easy lookup
- Consistent structure and formatting
- Minimal explanatory content

**Writing Guidelines**:
- Use consistent templates
- Include all parameters, properties, and options
- Provide brief, factual descriptions
- Focus on accuracy over explanation
- Make information scannable

### 4. Explanation (Understanding-Oriented)
**Purpose**: Help users understand concepts and context
**Characteristics**:
- Discuss topics broadly
- Provide background and context
- Explain design decisions and trade-offs
- Connect concepts to broader understanding

**Writing Guidelines**:
- Focus on why, not how
- Use examples to illustrate concepts
- Connect to users' existing knowledge
- Explore alternatives and implications
- Encourage deeper thinking

## Writing Style

### Voice and Tone
- Use active voice when possible
- Write in a conversational but professional tone
- Be direct and helpful
- Avoid unnecessary jargon and buzzwords
- Use "you" to address the reader directly

### Sentence Structure
- Keep sentences under 25 words when possible
- Use parallel structure in lists and series
- Vary sentence length for readability
- Start with the most important information

### Word Choice
- Use simple, common words over complex ones
- Define technical terms before using them
- Be specific rather than vague
- Avoid unnecessary qualifiers ("quite," "rather," "very")

## Language and Grammar

### Spelling and Capitalization
- Use American English spelling (e.g., "color" not "colour")
- Follow Merriam-Webster for spelling questions
- Use sentence-case capitalization for headings
- Capitalize proper nouns and trademarked terms correctly

### Punctuation
- Use the Oxford comma in series
- Place commas after introductory clauses
- Use commas before conjunctions joining independent clauses
- Avoid excessive punctuation for emphasis

### Abbreviations and Acronyms
- Spell out acronyms on first use: "Application Programming Interface (API)"
- Use full capitals without periods: "HTML" not "H.T.M.L."
- Use common Latin abbreviations (e.g., i.e., etc.) only in parentheses
- Avoid apostrophes in plurals: "APIs" not "API's"

### Numbers and Dates
- Spell out numbers one through nine, use numerals for 10 and above
- Use commas in numbers 10,000 and larger
- Use "January 1, 2024" format for dates
- Use "1990s" not "1990's" for decades

## Structure and Organization

### Page Structure
- Start with a clear, descriptive introduction
- Use descriptive headings that indicate content
- Follow logical information hierarchy
- Include "See also" sections for related content

### Heading Guidelines
- Use only one H1 per page (reserved for page title)
- Follow heading hierarchy: H2, H3, H4 (don't skip levels)
- Keep headings short and descriptive
- Use parallel construction for same-level headings
- Avoid single subsections

### Lists and Navigation
- Use bulleted lists for related items
- Use numbered lists for sequential steps
- Include lead-in sentences for lists
- Provide clear navigation between related pages

## Code Examples

### General Guidelines
- Include working, tested examples
- Show realistic use cases
- Explain what the code does and why
- Highlight key concepts being demonstrated

### Code Block Structure
1. **Heading**: Brief description of what the example demonstrates
2. **Context**: Explain the scenario and prerequisites
3. **Code**: Clean, well-commented code
4. **Explanation**: How the code works and key points
5. **Result**: What users should expect to see

### Code Formatting
- Use consistent indentation and formatting
- Include relevant imports and setup code
- Remove production credentials or sensitive data
- Test all code examples before publishing

## Formatting Guidelines

### Text Formatting
- Use **bold** for UI elements and important terms
- Use *italics* for emphasis and book titles
- Use `code formatting` for:
  - Code elements, variables, and functions
  - File names and paths
  - Command-line commands
  - API endpoints

### Links and References
- Use descriptive link text, not "click here"
- Link to relevant internal documentation first
- Verify external links regularly
- Avoid URL shorteners except for official organizational ones

### Images and Media
- Include descriptive alt text for all images
- Optimize image file sizes
- Use SVG for diagrams when possible
- Caption images and diagrams appropriately

## Accessibility and Inclusivity

### Inclusive Language
- Use gender-neutral language when gender is irrelevant
- Replace exclusionary terms:
  - "master/slave" → "primary/secondary" or "main/replica"
  - "whitelist/blacklist" → "allowlist/denylist"
  - "dummy" → "placeholder" or "sample"

### Spatial References
- Avoid directional language ("above," "below," "left," "right")
- Reference sections by name: "See the Installation section"
- Use descriptive phrases: "In the following example"

### Universal Design
- Write for various skill levels
- Provide multiple ways to access information
- Consider users with different devices and abilities
- Test with screen readers when possible

## SEO and Discoverability

### Content Optimization
- Write unique, substantial content (aim for 300+ words minimum)
- Use descriptive page titles and headings
- Include relevant keywords naturally
- Create comprehensive coverage of topics

### Metadata and Structure
- Write descriptive page titles
- Use structured markup when appropriate
- Create logical URL structures
- Maintain consistent navigation

### Avoiding SEO Pitfalls
- Don't keyword stuff or use irrelevant keywords
- Avoid duplicate content across pages
- Don't sacrifice readability for SEO
- Focus on user value over search rankings

## Review and Quality Assurance

### Content Review Checklist
- [ ] Is the information accurate and up-to-date?
- [ ] Does it follow the appropriate documentation type guidelines?
- [ ] Is the language clear and appropriate for the audience?
- [ ] Are code examples tested and working?
- [ ] Is the formatting consistent?
- [ ] Are all links working?
- [ ] Is the content accessible and inclusive?

### Editorial Process
1. **Draft**: Create initial content following these guidelines
2. **Technical Review**: Verify accuracy with subject matter experts
3. **Editorial Review**: Check style, grammar, and consistency
4. **User Testing**: Test with representative users when possible
5. **Publication**: Release and monitor for feedback
6. **Maintenance**: Regular updates and link checking

### Common Issues to Avoid
- Writing everything as a tutorial when other formats are more appropriate
- Mixing documentation types within a single page
- Using inconsistent terminology
- Assuming too much prior knowledge
- Forgetting to update related pages when making changes

## Reference Hierarchy

When questions arise not covered in this guide, consult resources in this order:

1. **Project-specific style**: Follow any project-specific guidelines first
2. **This guide**: Use these rules as the primary reference
3. **External references**:
   - Spelling: Merriam-Webster
   - Technical style: Microsoft Writing Style Guide
   - General style: Chicago Manual of Style

## Additional Resources

### Style Guides
- [Google Developer Documentation Style Guide](https://developers.google.com/style)
- [MDN Writing Guidelines](https://developer.mozilla.org/en-US/docs/MDN/Writing_guidelines)
- [Microsoft Writing Style Guide](https://docs.microsoft.com/en-us/style-guide/)

### Frameworks and Methods
- [Diátaxis](https://diataxis.fr/) - Documentation system architecture
- [Docs as Code](https://www.writethedocs.org/guide/docs-as-code/)

### Tools
- Grammar checkers (Grammarly, ProWritingAid)
- Link checkers
- Accessibility testing tools
- Code formatters and linters

---

*This guide is a living document. Update it regularly based on team feedback and evolving best practices.*
