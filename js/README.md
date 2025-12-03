# Elixir WASM JS Library

A minimalistic TypeScript library for Elixir WASM integration with iframe support.

## Setup

Install dependencies:

```bash
npm install
```

## Development

Build the library:

```bash
npm run build
```

Watch mode (rebuilds on changes):

```bash
npm run watch
```

## Structure

- `src/index.ts` - Main entry point
- `src/utils.ts` - Utility functions
- `src/iframe.ts` - Iframe creation and management
- `src/popcorn.ts` - Main Popcorn class with iframe bundling
- `dist/` - Generated output files

## Usage

```typescript
import { Popcorn, createPopcorn } from 'elixir-wasm-js';

const popcorn = createPopcorn({
  containerId: 'my-container',
  onReady: () => console.log('Ready!')
});

await popcorn.init();

const iframe = popcorn.createIframe();
document.body.appendChild(iframe);
```

## Features

- TypeScript support with type definitions
- Iframe bundling as virtual module (IIFE format)
- Minimal configuration with Rollup
- Watch mode for development
- ES module output
