# Readings — PHI 423 S2026

Each subfolder contains three files for one primary-source reading:

| File | Contents |
|------|----------|
| `scan.pdf` | Scan of the original printed text |
| `transcription.tex` | LaTeX transcription in Danish |
| `translation.tex` | LaTeX translation in English |

Transkribus (https://www.transkribus.org) is used to convert scans → initial transcriptions.

---

## Status

### `nielsen-darwin/` — R. Nielsen, "Et Synspunkt for Darwinismen" (1873)

- [x] **scan.pdf** — Present (Google Books scan of *For Idé og Virkelighed*, 1873, 11 pp.).
- [x] **transcription.tex** — Full Danish text extracted from PDF text layer; includes Agassiz passage (pp. 457–58) absent from earlier md draft.
- [ ] **translation.tex** — ⚠️ TODO: full English translation.

---

### `hoffding-darwin/` — H. Høffding, "Filosofien og Darwinismen" (1874)

Two-part article. Part I: *Nær og Fjern* nr. 93 (12. April 1874). Part II: nr. 94.

- [x] **scan-1.pdf** — Part I (COPY-DAN scan, image-only, 4 pp.).
- [x] **scan-2.pdf** — Part II (COPY-DAN scan, image-only, 4 pp.).
- [x] **transcription.tex** — Danish text for both parts, complete and verified against scans.
- [ ] **translation.tex** — ⚠️ PARTIAL: Part I translated; Part II still needed.

---

### `hoffding-realisme/` — H. Høffding, "Om Realisme i Videnskab og Tro" (1884)

- [x] **scan.pdf** — Present (scan of *Mindre Arbejder* vol. 1, iTextSharp PDF, 14 pp.).
- [x] **transcription.tex** — Full text from PDF text layer; substantially expanded over earlier md draft (Arkimedes analogy, Kant/Laplace/Darwin/Spencer sequence, physiology section, Kopernikus/Galilei/Darwin list, Martensen passage, theology-and-medicine parallel, full closing on *Blaserthed*).
- [x] **translation.tex** — English translation complete (based on earlier md draft; may need updating against expanded transcription).

---

### `brandes-dyret/` — G. Brandes, "Dyret i Mennesket" (1890)

- [x] **scan.pdf** — Present (Google Books scan of *Samlede Skrifter*, vol. VII, 19 pp.).
- [x] **transcription.tex** — Full Danish text extracted from PDF text layer (no Transkribus needed).
- [ ] **translation.tex** — ⚠️ TODO: full English translation.

---

## Transkribus workflow

1. Upload `scan.pdf` to Transkribus and run HTR.
2. Export the result as plain text or PAGE XML.
3. Paste into `transcription.tex` and correct OCR errors manually.
4. Commit corrected transcription before starting translation.
