from collections import Counter
from dataclasses import dataclass
import re

in_file = "./stark_verifier.instructions"
out_file = "./common_sequences.md"

@dataclass
class Sequence:
    incidence: int
    length: int
    tokens: str

def find_most_common_sequences(text, sequence_length):
    words = re.findall(r'\w+', text.lower())
    sequences = [' '.join(words[i:i+sequence_length]) for i in range(len(words) - sequence_length + 1)]
    most_common = Counter(sequences).most_common()[:15]
    return [Sequence(incidence=incidence, length=sequence_length, tokens=tokens) for (tokens, incidence) in most_common]

if __name__ == "__main__":
    with open(in_file, "r") as file:
        text = file.read()

    sequences = []
    for sequence_length in range(21, 1, -1):
        for new_sequence in find_most_common_sequences(text, sequence_length):
            include_new_sequence = True
            for existing_sequence in sequences:
                if new_sequence.tokens in existing_sequence.tokens:
                    if new_sequence.incidence <= 1.1 * existing_sequence.incidence:
                        include_new_sequence = False
            if include_new_sequence:
                sequences += [new_sequence]
    sequences.sort(key=lambda sequence: -sequence.incidence)

    with open(out_file, 'w') as file:
        file.write(f"| incidence | len | sequence |\n")
        file.write(f"|----------:|----:|:---------|\n")
        for sequence in sequences:
            file.write(f"| {sequence.incidence:>9} | {sequence.length:>3} | {sequence.tokens} |\n")
