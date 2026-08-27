#!/bin/bash
# Copy image and bibliography assets from vtfys_2025 into vtfys_2026.

SRC="$(dirname "$0")/../vtfys_2025"
DST="$(dirname "$0")"

files=(
    aspects.jpeg
    carnap.jpeg
    carnap.pdf
    decades.jpeg
    deregt.jpg
    dieks.jpg
    frege.jpeg
    hempel.jpeg
    image.jpg
    vortices.jpeg
    refs.bib
)

for f in "${files[@]}"; do
    if [ -f "$SRC/$f" ]; then
        cp "$SRC/$f" "$DST/$f"
        echo "Copied $f"
    else
        echo "WARNING: $SRC/$f not found"
    fi
done

echo "Done."
