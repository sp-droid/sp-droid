## Basic way to encrypt your stuff

### One time setup
1. install python, uv
2. uv init --bare, uv add cryptography inquirerpy

### Running
Run the script from the directory containing the files:

```sh
uv run main.py
```

Choose whether to encrypt or decrypt, then select a file from the list. The
encryption option lists `.txt` files, while the decryption option lists
`.txt.enc` files created by the script.