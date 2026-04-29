import sys
import re
import itertools

def main():
    shuangpin_file = "zk-py-shuangpin.el"
    phrases_file = "zk-py-shuangpin-phrases-list.txt"
    output_file = "phrases-index.tmp"

    # 1. Parse shuangpin file to build a character-to-encodings mapping
    encoding_to_chars = {}
    char_to_encodings = {}
    try:
        with open(shuangpin_file, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        print(f"Error reading {shuangpin_file}: {e}")
        return

    # Extract patterns like ("oa" "阿啊...")
    matches = re.findall(r'\("([^"]+)"\s+"([^"]+)"\)', content)
    for enc, chars in matches:
        encoding_to_chars[enc] = chars
        for char in chars:
            if char not in char_to_encodings:
                char_to_encodings[char] = []
            if enc not in char_to_encodings[char]:
                char_to_encodings[char].append(enc)

    # 2. Parse phrases and map to encodings
    encoding_to_phrases = {}
    try:
        with open(phrases_file, 'r', encoding='utf-8') as f:
            for line in f:
                phrase = line.strip()
                if not phrase:
                    continue
                
                # Get all possible encodings for each character in the phrase
                phrase_char_encs = []
                stop_phrase = False
                for char in phrase:
                    if char not in char_to_encodings:
                        # Requirement: print phrase and character and quit if not found
                        print(f"Phrase: {phrase}, Character: {char}")
                        sys.exit(1)
                    phrase_char_encs.append(char_to_encodings[char])
                
                # Generate all permutations (Cartesian product) of the encodings
                for p in itertools.product(*phrase_char_encs):
                    combined_enc = "".join(p)
                    if combined_enc not in encoding_to_phrases:
                        encoding_to_phrases[combined_enc] = []
                    phrases = encoding_to_phrases[combined_enc]
                    if phrase not in phrases:
                        phrases.append(phrase)
                        # TODO: sort phrases by comparing character by
                        # character.  The relative order of two
                        # characters is determined by their positions
                        # in encoding_to_chars[enc], where enc is the
                        # char encoding from combined_enc
    except Exception as e:
        print(f"Error reading {phrases_file}: {e}")
        return

    # 3. Output results in Elisp format
    try:
        with open(output_file, 'w', encoding='utf-8') as f:
            # Sort by encoding for consistent output
            for enc in sorted(encoding_to_phrases.keys()):
                phrases = encoding_to_phrases[enc]
                # If the encoding has only one candidate, add a fake
                # one so that the user always need to manually commit
                # it.  Otherwise, Quail will autocommit which is
                # undesirable.
                if len(phrases) < 2:
                    phrases.append("_")
                phrases_formatted = " ".join([f'"{p}"' for p in phrases])
                f.write(f'("{enc}" [{phrases_formatted}])\n')
    except Exception as e:
        print(f"Error writing to {output_file}: {e}")
        return

if __name__ == "__main__":
    main()
