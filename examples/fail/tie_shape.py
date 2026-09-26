import torch.nn as nn


# the projection maps vocab to d, so its weight is [d, vocab], not the
# embedding's [vocab, d]
# expect-error: in Tied.__init__, `self.out.weight = self.embed.weight`
# expect-error: expected out_features = d, got vocab
class Tied(nn.Module):
    def __init__(self, vocab: int, d: int):
        super().__init__()
        self.embed = nn.Embedding(vocab, d)
        self.out = nn.Linear(vocab, d, bias=False)
        self.out.weight = self.embed.weight
