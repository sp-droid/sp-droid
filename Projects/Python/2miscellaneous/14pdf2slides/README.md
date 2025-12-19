```bash
conda create -n pdf2slides python=3.12
pip install pdf2slides
```

In the file at `miniconda3\envs\pdf2slides\Lib\site-packages\pdf2slides\converter.py`

Substitute the import `PPStructure` for `PPStructureV3 as PPStructure`



Put all pdfs you want in the input folder, execute script in the correct environment.