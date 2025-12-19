from pathlib import Path
from pdf2slides import Converter

converter = Converter()

for pdf in Path("input").rglob("*.pdf"):
    print(f"Converting {pdf} to {Path('output') / f'{pdf.stem}.pptx'}...")
    converter.convert(str(pdf), str(Path("output") / f"{pdf.stem}.pptx"))

print("Conversion complete!")
input("Press Enter to exit...")