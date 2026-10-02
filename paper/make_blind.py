"""Generate the anonymised manuscript for double-anonymised review.

Reads paper/maintext.qmd and writes paper_blind/maintext.qmd (never edit that copy by hand):
masks self-citations via apaquarto's `mask` option and removes the repository link.
Run from the repository root or from paper/: python3 paper/make_blind.py
"""
import pathlib, re, shutil

HERE = pathlib.Path(__file__).resolve().parent
OUT = HERE.parent / "paper_blind"
SELF_CITES = ["pampaka2012associ", "pampaka2016mathem", "zhang2025Belief", "zhang2026reusaba"]
REPO = re.compile(r"<?https://github\.com/lukezed/Bayesian_MLM_connectionist/?>?")

src = (HERE / "maintext.qmd").read_text()
mask = "mask: true\nmasked-citations:\n" + "".join(f"  - {k}\n" for k in SELF_CITES)
yaml_end = src.index("\nformat:")
blind = src[:yaml_end + 1] + mask + src[yaml_end + 1:]
blind = REPO.sub("[repository link masked for review]", blind)

OUT.mkdir(exist_ok=True)
(OUT / "maintext.qmd").write_text(blind)
shutil.copy(HERE / "reference.bib", OUT / "reference.bib")

# self-check: nothing identifying survives in the source
assert "lukezed" not in blind, "repository link survived"
assert "mask: true" in blind, "mask option missing"
assert all(k in blind for k in SELF_CITES), "a masked key is no longer cited; update SELF_CITES"
print("paper_blind/maintext.qmd written")
