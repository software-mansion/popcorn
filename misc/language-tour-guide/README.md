# Elixir Language Tour Guide

An interactive, browser-based tutorial application for learning the Elixir programming language. This project combines React with WebAssembly to run Elixir code directly in the browser via the [Popcorn project](https://popcorn.swmansion.com/), providing an immersive learning experience without any server-side execution.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Folder Structure](#folder-structure)
- [Getting Started](#getting-started)
- [Environment Variables](#environment-variables)
- [Content Management](#content-management)
- [Error Tracking](#error-tracking)

---

## Overview

The Elixir Language Tour Guide is a platform that provides:

- **Interactive code examples** written in Elixir
- **Real-time code execution** in the browser using WebAssembly (via [Popcorn](https://popcorn.swmansion.com/))
- **Structured learning path** covering Elixir fundamentals

---

## Architecture

### Component Architecture

- **Context Providers**: Manage Popcorn WebAssembly runtime lifecycle
- **State Management**: Zustand stores for code editor, execution history, and navigation
- **Routing**: React Router with dynamically generated routes from MDX content
- **Content Processing**: Custom Remark/Rehype plugins for MDX transformation
- **Storage**: Browser LocalStorage for code persistence

---

## Folder Structure

- **`src/content/`**: Contains all educational content as MDX files. Organized in numbered chapters with descriptive subdirectories. Each MDX file represents a single lesson page.

- **`src/plugins/`**: Custom Remark and Rehype plugins that transform MDX content during build:
  - `remarkCollectCode.ts`: Extracts `<EditorCode>` blocks from MDX and exports as `defaultCode`
  - `rehypeRawCode.ts`: Preserves raw code text for copying functionality

- **`src/utils/content/`**: Content processing utilities that build navigation trees, generate routes dynamically, and load MDX modules at runtime.

- **`elixir_tour/`**: Mix project that compiles Elixir code to WebAssembly using [Popcorn](https://popcorn.swmansion.com/).

- **`public/wasm/`**: Generated directory (gitignored) containing WebAssembly binaries and runtime created during the build process.

---

## Getting Started

### Prerequisites

- **Node.js**: 18.x or higher
- **npm**: 9.x or higher (or compatible package manager)
- **Mix**: Elixir build tool (for building WebAssembly)
- **Elixir**: 1.17

### Installation

1. **Install dependencies:**

```bash
npm install
```

2. **Set up environment variables** (see [Environment Variables](#environment-variables) section):

```bash
cp .env.example .env
# Edit .env with your configuration
```

3. **Start the development server:**

```bash
npm run dev
```

The application will be available at `http://localhost:5173`.

---

## Environment Variables

Create a `.env` file in the project root with the following variables:

```bash
# Sentry Configuration
VITE_SENTRY_DSN=<your_sentry_dsn>
VITE_MODE="development" | "production"

# Base URL for deployment (Optional - defaults to "/" for local dev)
# Set in vite.config.ts as base: "/language_tour_guide"
# This is read automatically by Vite as import.meta.env.BASE_URL
```

### Notes

- Sentry configuration is initialized in `src/utils/sentry/index.ts`.
- If `VITE_SENTRY_DSN` is not set, the app will still function normally but won't report errors.

---

## Content Management

### Adding New Lessons

1. **Create an MDX file** in the appropriate chapter directory under `src/content/`:

````markdown
export const frontmatter = { order: 10 };

# Your Lesson Title

Lesson content goes here...

<EditorCode>
```elixir 
IO.puts("Example code")
```
</EditorCode>
````

2. **Set the `order` frontmatter** to control lesson sequence within the chapter.

3. **Use `<EditorCode>` component** to define interactive code blocks that appear in the editor.

4. The route and navigation will be generated automatically based on:
   - File path: `/src/content/1-basic-types/atoms.mdx` → `/1-basic-types/atoms`
   - Frontmatter `order` property for sorting

### Content Structure Guidelines

- **Chapter directories**: Use numbered prefixes (e.g., `1-basic-types/`, `2-data-structures/`)
- **File naming**: Use kebab-case (e.g., `pattern-matching.mdx`)
- **Frontmatter**: Include `order` property for sorting purpose
- **Code blocks**: Use `<EditorCode>` for interactive examples which appear in code editor.

### MDX Processing Pipeline

1. **Remark phase** (`remarkCollectCode`):
   - Finds all `<EditorCode>` blocks
   - Extracts Elixir code
   - Removes blocks from rendered output
   - Exports combined code as `defaultCode` variable

2. **Rehype phase** (`rehypeRawCode`):
   - Adds raw code text to `<pre>` elements
   - Enables copy-to-clipboard functionality

3. **Route generation** (`route-builder.tsx`):
   - Dynamically imports all MDX modules
   - Creates React Router routes
   - Wraps content in `MdxWrapper` with code editor

---

## Error Tracking

### Sentry Integration

The application uses Sentry for error and crash tracking. Configuration in `src/utils/sentry/index.ts`:

```typescript
initSentry(); // Called in main.tsx
```

### Tracked Events

1. **Code Exceptions**: Errors from code execution
   - Tag: `code-exception`
   - Includes code and error message

2. **AtomVM Crashes**: WebAssembly runtime crashes
   - Tag: `atomvm-crash`
   - Includes code, stdout, stderr
