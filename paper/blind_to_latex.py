"""Turn the rendered blind .tex into a self-contained LaTeX submission (Elsevier source file).

Run after rendering paper_blind/maintext.qmd with `quarto render maintext.qmd --to wjschne/apaquarto-pdf`
(keep-tex: true). Writes paper_blind/latex/{maintext.tex, Fig1.png, Fig2.png}, all in one folder.
- removes the author block that apaquarto keeps in the source even under `mask`
- loads TeX Gyre Termes by file name (Times New Roman is absent on Linux build servers)
- converts non-ASCII characters to TeX code and declares xelatex on line 1
"""
import pathlib, re, shutil

ROOT = pathlib.Path(__file__).resolve().parent.parent
SRC, OUT = ROOT / "paper_blind" / "maintext.tex", ROOT / "paper_blind" / "latex"

t = SRC.read_text()
t = re.sub(r"\\authorsnames.*?(?=\\abstract\{)", "", t, count=1, flags=re.S)
t = re.sub(r"\\authornote\{.*?(?=\\makeatletter)", "", t, count=1, flags=re.S)
t = re.sub(r"\\setmainfont\[[^\]]*\]\{Times New Roman\}",
           r"\\setmainfont{texgyretermes}[Extension=.otf,UprightFont=*-regular,BoldFont=*-bold,ItalicFont=*-italic,BoldItalicFont=*-bolditalic]", t)
t = t.replace("../figures/", "")
for a, b in {"R̂": r"\(\hat{R}\)", "R²": r"R\textsuperscript{2}", "²": r"\textsuperscript{2}", "β": r"\(\beta\)",
             "×": r"\(\times\)", "≈": r"\(\approx\)", "‐": "-", "–": "--", "—": "---",
             "ü": r"\"{u}", "ö": r"\"{o}", "é": r"\'{e}", "í": r"\'{\i}", "å": r"\aa{}", "ø": r"\o{}", "Ø": r"\O{}"}.items():
    t = t.replace(a, b)
t = "%!TEX TS-program = xelatex\n" + t

leftover = sorted(set(re.findall(r"[^\x00-\x7F]", t)))
assert not leftover, f"unconverted characters: {leftover}"
for pat in ["Pampaka", "Chi Zhang", "Manchester", "East China Normal", "lukezed", "ORCID", "Times New Roman"]:
    assert pat not in t, f"{pat} survived in the LaTeX source"

OUT.mkdir(exist_ok=True)
(OUT / "maintext.tex").write_text(t)
for f in ("Fig1.png", "Fig2.png"):
    shutil.copy(ROOT / "figures" / f, OUT / f)
print("paper_blind/latex written")
