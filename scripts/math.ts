// scripts/math.ts
import { readLines } from "https://deno.land/std@0.224.0/io/mod.ts";
import katex from "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.mjs";

for await (const line of readLines(Deno.stdin)) {
  try {
    const DISPLAY = ":DISPLAY ";
    const useDisplay = line.startsWith(DISPLAY);
    const expr = useDisplay ? line.substring(DISPLAY.length) : line;

    // Render and force single line (Pandoc expects one line per input)
    const html = katex
      .renderToString(expr, { displayMode: useDisplay, strict: "warn", throwOnError: false })
      .replaceAll("\n", "");

    console.log(html);
  } catch (err) {
    // Fail fast with context (shows the bad input)
    console.error("KaTeX error for:", line);
    throw err;
  }
}