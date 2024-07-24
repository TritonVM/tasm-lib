from collections import Counter
import re

def find_most_common_sequences(text, sequence_length):
    words = re.findall(r'\w+', text.lower())
    sequences = [' '.join(words[i:i+sequence_length]) for i in range(len(words) - sequence_length + 1)]
    sequence_counts = Counter(sequences)
    return sequence_counts.most_common()[:15]

with open("./stark_verifier.instructions", "r") as file:
    text = file.read()

out_file = "./common_sequences.txt"
with open(out_file, 'w') as file:
    pass  # reset file content

for sequence_length in range(2, 20):
    most_common = find_most_common_sequences(text, sequence_length)
    with open(out_file, "a") as file:
        file.write(f"For sequence length {sequence_length}:\n")
        for (sequence, num) in most_common:
            file.write(f"  {num:>7} {sequence}\n")
