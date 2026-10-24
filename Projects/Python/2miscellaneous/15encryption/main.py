"""
Encrypt or decrypt a text file with a password.

Run the script without arguments and use the interactive prompts to choose
an operation and a file.

Requires:  pip install "cryptography>=44" InquirerPy

FILE FORMAT (write this down; it's what you need to rebuild the script):
    bytes 0-2    magic   b"PV2"
    bytes 3-18   salt    16 random bytes
    bytes 19-30  nonce   12 random bytes
    bytes 31-    AES-256-GCM ciphertext (includes 16-byte auth tag at the end)
                 the magic b"PV2" is passed as associated data

    key = Argon2id(password UTF-8, salt, iterations=3, lanes=4,
                   memory_cost=65536 KiB (64 MiB), length=32)

None of the above is secret. Only the password is.
"""

import getpass
import os
import sys
from pathlib import Path

from InquirerPy import inquirer
from cryptography.exceptions import InvalidTag
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.argon2 import Argon2id

MAGIC = b"PV2"
SALT_LEN = 16
NONCE_LEN = 12


def derive_key(password: str, salt: bytes) -> bytes:
    return Argon2id(
        salt=salt, length=32, iterations=3, lanes=4, memory_cost=65536
    ).derive(password.encode("utf-8"))


def encrypt(path: str) -> None:
    out = path + ".enc"
    if os.path.exists(out):
        sys.exit(f"{out} already exists; move it first.")

    with open(path, "rb") as f:
        plaintext = f.read()

    password = getpass.getpass("Password: ")
    if password != getpass.getpass("Repeat password: "):
        sys.exit("Passwords don't match.")

    salt = os.urandom(SALT_LEN)
    nonce = os.urandom(NONCE_LEN)
    ciphertext = AESGCM(derive_key(password, salt)).encrypt(nonce, plaintext, MAGIC)

    with open(out, "wb") as f:
        f.write(MAGIC + salt + nonce + ciphertext)
    print(f"Wrote {out}. Check that decrypt works before deleting {path}.")


def decrypt(path: str) -> None:
    out = path[:-4] if path.endswith(".enc") else path + ".dec"
    if os.path.exists(out):
        sys.exit(f"{out} already exists; move it first.")

    with open(path, "rb") as f:
        data = f.read()
    if not data.startswith(MAGIC):
        sys.exit("Not a vault file.")

    salt = data[3:3 + SALT_LEN]
    nonce = data[3 + SALT_LEN:3 + SALT_LEN + NONCE_LEN]
    ciphertext = data[3 + SALT_LEN + NONCE_LEN:]

    password = getpass.getpass("Password: ")
    try:
        plaintext = AESGCM(derive_key(password, salt)).decrypt(nonce, ciphertext, MAGIC)
    except InvalidTag:
        sys.exit("Wrong password (or the file is damaged).")

    with open(out, "wb") as f:
        f.write(plaintext)
    print(f"Wrote {out}.")


def choose_operation() -> str:
    return inquirer.select(
        message="What would you like to do?",
        choices=[
            {"name": "Encrypt a text file", "value": "encrypt"},
            {"name": "Decrypt an encrypted text file", "value": "decrypt"},
        ],
    ).execute()


def choose_file(operation: str) -> Path:
    pattern = "*.txt" if operation == "encrypt" else "*.txt.enc"
    files = sorted(Path.cwd().glob(pattern), key=lambda path: path.name.lower())

    if not files:
        description = ".txt files" if operation == "encrypt" else ".txt.enc files"
        sys.exit(f"No {description} found in {Path.cwd()}.")

    return inquirer.select(
        message="Which file would you like to use?",
        choices=[{"name": path.name, "value": path} for path in files],
    ).execute()


if __name__ == "__main__":
    operation = choose_operation()
    selected_file = choose_file(operation)
    {"encrypt": encrypt, "decrypt": decrypt}[operation](str(selected_file))