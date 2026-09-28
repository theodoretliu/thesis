import torch.nn as nn


# the head maps vocab to d, so its weight is [d, vocab], not the embedding's
# [vocab, d], which the dict's entry has
# expect-error: in LM.__init__, `self.parts.embed.weight = self.head.weight`
# expect-error: torch.nn.Embedding.weight (assigned)
# expect-error: expected num_embeddings = vocab, got d
class LM(nn.Module):
    def __init__(self, vocab: int, d: int):
        super().__init__()
        self.parts = nn.ModuleDict(dict(embed=nn.Embedding(vocab, d), norm=nn.LayerNorm(d)))
        self.head = nn.Linear(vocab, d, bias=False)
        self.parts.embed.weight = self.head.weight
