import json

class CharacterTokenizer:
    """Minimal character-level tokenizer, efficient for large texts."""

    def fit(self, text):
        self.chars = sorted(set(text))
        self.compute_transforms()

    def compute_transforms(self):
        self.stoi = {c: i for i, c in enumerate(self.chars)}
        self.itos = {i: c for i, c in enumerate(self.chars)}
        self._vocab_size = len(self.chars)
    
    @property
    def vocab_size(self):
        return self._vocab_size

    def save(self, stem_path):
        # Write the list of characters as JSON to preserve special characters
        with open(f"{stem_path}.json", "w", encoding="utf-8") as file:
            json.dump(self.chars, file, ensure_ascii=False)
            
    def load(self, stem_path):
        # Read the list of characters from JSON
        with open(f"{stem_path}.json", "r", encoding="utf-8") as file:
            self.chars = json.load(file)
        self.compute_transforms()

    def encode(self, text):
        return list(map(self.stoi.__getitem__, text))

    def decode(self, ids):
        return ''.join(map(self.itos.__getitem__, ids))