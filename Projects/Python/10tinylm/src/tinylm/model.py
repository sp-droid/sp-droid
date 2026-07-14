"""The RoPE/SwiGLU decoder-only character transformer."""

from __future__ import annotations

import math

import torch
from torch import nn
from torch.nn import functional as F


class RotaryPositionalEmbedding(nn.Module):
    def __init__(self, dim: int, base: int = 10_000):
        super().__init__()
        inv_freq = 1.0 / (base ** (torch.arange(0, dim, 2, dtype=torch.float32) / dim))
        self.register_buffer("inv_freq", inv_freq, persistent=False)
        self.register_buffer("cos_cached", torch.empty(0), persistent=False)
        self.register_buffer("sin_cached", torch.empty(0), persistent=False)

    def _cache(self, sequence_length: int, device: torch.device) -> None:
        if self.cos_cached.size(0) >= sequence_length and self.cos_cached.device == device:
            return
        positions = torch.arange(sequence_length, dtype=torch.float32, device=device)
        frequencies = torch.outer(positions, self.inv_freq.to(device))
        embedding = torch.cat((frequencies, frequencies), dim=-1)
        self.cos_cached = embedding.cos()
        self.sin_cached = embedding.sin()

    @staticmethod
    def _rotate_half(values: torch.Tensor) -> torch.Tensor:
        first, second = values.chunk(2, dim=-1)
        return torch.cat((-second, first), dim=-1)

    def forward(self, query: torch.Tensor, key: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        length = query.size(-2)
        self._cache(length, query.device)
        cos = self.cos_cached[:length].to(query.dtype)[None, None, :, :]
        sin = self.sin_cached[:length].to(query.dtype)[None, None, :, :]
        return query * cos + self._rotate_half(query) * sin, key * cos + self._rotate_half(key) * sin


class MultiHeadSelfAttention(nn.Module):
    def __init__(self, n_embed: int, n_heads: int):
        super().__init__()
        self.n_heads = n_heads
        self.head_size = n_embed // n_heads
        self.qkv = nn.Linear(n_embed, 3 * n_embed)
        self.proj = nn.Linear(n_embed, n_embed)
        self.rope = RotaryPositionalEmbedding(self.head_size)

    def forward(self, values: torch.Tensor) -> torch.Tensor:
        batch, length, channels = values.shape
        qkv = self.qkv(values).view(batch, length, 3, self.n_heads, self.head_size)
        query, key, value = qkv.permute(2, 0, 3, 1, 4)
        query, key = self.rope(query, key)
        output = F.scaled_dot_product_attention(query, key, value, is_causal=True)
        output = output.transpose(1, 2).contiguous().view(batch, length, channels)
        return self.proj(output)


class SwiGLU(nn.Module):
    def __init__(self, n_embed: int, hidden_dim: int):
        super().__init__()
        self.w_gate = nn.Linear(n_embed, hidden_dim, bias=False)
        self.w_up = nn.Linear(n_embed, hidden_dim, bias=False)
        self.w_down = nn.Linear(hidden_dim, n_embed, bias=False)

    def forward(self, values: torch.Tensor) -> torch.Tensor:
        return self.w_down(F.silu(self.w_gate(values)) * self.w_up(values))


class TransformerBlock(nn.Module):
    def __init__(self, n_embed: int, n_heads: int):
        super().__init__()
        self.attention = MultiHeadSelfAttention(n_embed, n_heads)
        self.feed_forward = SwiGLU(n_embed, 4 * n_embed)
        self.ln1 = nn.RMSNorm(n_embed, eps=1e-6)
        self.ln2 = nn.RMSNorm(n_embed, eps=1e-6)

    def forward(self, values: torch.Tensor) -> torch.Tensor:
        values = values + self.attention(self.ln1(values))
        return values + self.feed_forward(self.ln2(values))


class GPTLLM(nn.Module):
    def __init__(self, vocab_size: int, n_embed: int, n_heads: int, n_layers: int, block_size: int):
        super().__init__()
        self.block_size = block_size
        self.token_embedding = nn.Embedding(vocab_size, n_embed)
        self.blocks = nn.ModuleList([TransformerBlock(n_embed, n_heads) for _ in range(n_layers)])
        self.final_norm = nn.RMSNorm(n_embed, eps=1e-6)
        self.lm_head = nn.Linear(n_embed, vocab_size, bias=False)

    def forward(self, input_ids: torch.Tensor) -> torch.Tensor:
        if input_ids.size(1) > self.block_size:
            raise ValueError(f"Sequence length {input_ids.size(1)} exceeds block size {self.block_size}")
        values = self.token_embedding(input_ids)
        for block in self.blocks:
            values = block(values)
        return self.lm_head(self.final_norm(values))

    def loss(self, input_ids: torch.Tensor, targets: torch.Tensor, reduction: str = "mean") -> torch.Tensor:
        logits = self(input_ids)
        return F.cross_entropy(logits.reshape(-1, logits.size(-1)), targets.reshape(-1), reduction=reduction)

    @torch.inference_mode()
    def sample_next_token(
        self,
        input_ids: torch.Tensor,
        temperature: float = 0.0,
        generator: torch.Generator | None = None,
    ) -> torch.Tensor:
        """Sample one token from the final position of the cropped context."""
        context = input_ids[:, -self.block_size :]
        logits = self(context)[:, -1, :]
        if temperature <= 0:
            return torch.argmax(logits, dim=-1, keepdim=True)
        probabilities = F.softmax(logits / temperature, dim=-1)
        return torch.multinomial(probabilities, num_samples=1, generator=generator)

    @torch.inference_mode()
    def generate(
        self,
        input_ids: torch.Tensor,
        max_new_tokens: int,
        temperature: float = 0.0,
        generator: torch.Generator | None = None,
    ) -> torch.Tensor:
        """Generate with strict context cropping; temperature zero is greedy and deterministic."""
        generated = input_ids
        for _ in range(max_new_tokens):
            next_token = self.sample_next_token(generated, temperature=temperature, generator=generator)
            generated = torch.cat((generated, next_token), dim=1)
        return generated


def build_model(model_config: dict[str, object], vocab_size: int) -> GPTLLM:
    return GPTLLM(
        vocab_size=vocab_size,
        n_embed=int(model_config["n_embed"]),
        n_heads=int(model_config["n_heads"]),
        n_layers=int(model_config["n_layers"]),
        block_size=int(model_config["block_size"]),
    )


def parameter_counts(model: nn.Module) -> dict[str, int | float]:
    total = sum(parameter.numel() for parameter in model.parameters())
    trainable = sum(parameter.numel() for parameter in model.parameters() if parameter.requires_grad)
    return {
        "total": total,
        "trainable": trainable,
        "total_m": total / 1_000_000,
        "trainable_m": trainable / 1_000_000,
    }
