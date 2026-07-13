"""A compact character tokenizer with explicit BOS and unknown tokens."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Iterable


class CharacterTokenizer:
    BOS = "<bos>"
    UNK = "<unk>"
    SPECIAL_TOKENS = (BOS, UNK)

    def __init__(self, tokens: list[str]):
        if tokens[: len(self.SPECIAL_TOKENS)] != list(self.SPECIAL_TOKENS):
            raise ValueError("Tokenizer must begin with <bos> and <unk> tokens")
        if len(tokens) != len(set(tokens)):
            raise ValueError("Tokenizer tokens must be unique")
        self.tokens = tokens
        self.stoi = {token: index for index, token in enumerate(tokens)}
        self.itos = {index: token for index, token in enumerate(tokens)}

    @classmethod
    def fit_chunks(cls, chunks: Iterable[str]) -> "CharacterTokenizer":
        chars: set[str] = set()
        for chunk in chunks:
            chars.update(chunk)
        return cls([*cls.SPECIAL_TOKENS, *sorted(chars)])

    @property
    def vocab_size(self) -> int:
        return len(self.tokens)

    @property
    def bos_id(self) -> int:
        return self.stoi[self.BOS]

    @property
    def unk_id(self) -> int:
        return self.stoi[self.UNK]

    def encode(self, text: str) -> tuple[list[int], int]:
        """Encode text and return the number of characters mapped to <unk>."""
        ids: list[int] = []
        unknown_count = 0
        for char in text:
            token_id = self.stoi.get(char)
            if token_id is None:
                token_id = self.unk_id
                unknown_count += 1
            ids.append(token_id)
        return ids, unknown_count

    def decode(self, ids: Iterable[int], include_special_tokens: bool = False) -> str:
        decoded: list[str] = []
        for token_id in ids:
            token = self.itos[int(token_id)]
            if include_special_tokens or token not in self.SPECIAL_TOKENS:
                decoded.append(token)
        return "".join(decoded)

    def to_dict(self) -> dict[str, object]:
        return {"version": 1, "tokens": self.tokens}

    def save(self, path: str | Path) -> None:
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(self.to_dict(), ensure_ascii=False, indent=2), encoding="utf-8")

    @classmethod
    def load(cls, path: str | Path) -> "CharacterTokenizer":
        data = json.loads(Path(path).read_text(encoding="utf-8"))
        return cls(list(data["tokens"]))
