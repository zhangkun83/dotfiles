#!/bin/bash
set -e
python3 zk-py-shuangpin-generate-phrases-index.py

cat <<EOF > zk-py-shuangpin.el
;; GENERATED FILE.  DO NOT MODIFY.
;; Use zk-py-shuangpin-rebuild.sh to regenerate this file.
EOF

cat zk-py-shuangpin.el.base phrases-index.tmp >> zk-py-shuangpin.el

cat <<EOF >> zk-py-shuangpin.el
)

(provide 'zk-py-shuangpin)
EOF


# Insert a space at the end of the input sequence to force the user to
# press a SPACE before committing the candidate.  Otherwise, if there is
# only one candidate, quail's behavior is to autocommit without confirmation.
# Changes " ("wo" "我")
#
# 1. Find the range starting from "(quail-define-rules" to the end of the file
# 2. Within that range, look for lines starting with optional spaces, a quote, and lowercase letters
# 3. Use a backreference to insert the space after the letters
sed -i '/(quail-define-rules/,$ s/^\( *"[a-z]\+\)\([^ ]\)/\1 \2/' zk-py-shuangpin.el

echo "zk-py-shuangpin.el regenerated for pharses"
rm phrases-index.tmp
